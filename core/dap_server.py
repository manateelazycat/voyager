#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2023 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import json
import queue
import re
import subprocess
from subprocess import PIPE
from sys import stderr
from threading import Thread
import threading

from core.utils import *

DEFAULT_BUFFER_SIZE = 100000000  # we need make buffer size big enough, avoid pipe hang by big data response from DAP server

class DapServer:

    def __init__(self):
        self.dap_subprocess = subprocess.Popen(["python", "-m" "debugpy", "--listen", "5678", "/home/andy/test.py"],
                                               bufsize=DEFAULT_BUFFER_SIZE,
                                               stdin=PIPE,
                                               stdout=PIPE,
                                               stderr=stderr)

        self.receiver = DapServerReceiver(self.dap_subprocess)
        self.receiver.start()

        self.sender = DapServerSender(self.dap_subprocess)
        self.sender.start()

        self.receive_message_thread = threading.Thread(target=self.dap_message_dispatcher)
        self.receive_message_thread.start()

    def dap_message_dispatcher(self):
        try:
            while True:
                message = self.receiver.get_message()
                print(message["content"])
        except:
            import traceback
            print(traceback.format_exc())

class MessageSender(Thread):

    def __init__(self, process: subprocess.Popen):
        super().__init__()

        self.process = process
        self.queue = queue.Queue()

    def send_request(self, message):
        self.queue.put(message)

class MessageReceiver(Thread):

    def __init__(self, process: subprocess.Popen):
        super().__init__()

        self.process = process
        self.queue = queue.Queue()

    def get_message(self):
        return self.queue.get(block=True)

class DapServerSender(MessageSender):

    def __init__(self, process: subprocess.Popen):
        super().__init__(process)

    def send_message(self, message: dict):
        json_content = json.dumps(message)

        message_str = "Content-Length: {}\r\n\r\n{}".format(len(json_content), json_content)

        self.process.stdin.write(message_str.encode("utf-8"))    # type: ignore
        self.process.stdin.flush()    # type: ignore

    def run(self) -> None:
        try:
            while self.process.poll() is None:
                message = self.queue.get()
                self.send_message(message)
        except:
            import traceback
            print(traceback.format_exc())

class DapServerReceiver(MessageReceiver):

    def __init__(self, process: subprocess.Popen):
        super().__init__(process)

    def emit_message(self, line):
        if not line:
            return
        try:
            # Send message.
            self.queue.put({
                "name": "dap_recv_message",
                "content": parse_json_content(line)
            })
        except:
            import traceback
            print(traceback.format_exc())

    def run(self):
        try:
            content_length = None
            buffer = bytearray()
            while self.process.poll() is None:
                if content_length is None:
                    match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                    if match is not None:
                        end = match.end()
                        parts = match.group(0).decode("utf-8").strip().split(": ")
                        content_length = int(parts[1])

                        buffer = buffer[end:]
                    else:
                        line = self.process.stdout.readline()    # type: ignore
                        # dart_analysis_server 会发送 Content-Type,
                        # 导致解析的 json 回包内容不完整
                        if re.search(b"Content-Type", line) is None:
                            buffer = buffer + line
                else:
                    if len(buffer) < content_length:
                        # 这个检查算是个防御吧，实际应该用不到了。先保留，后续再看。
                        match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                        if match is not None:
                            start = match.start()
                            msg = buffer[0:start]
                            buffer = buffer[start:]
                            content_length = None
                            self.emit_message(msg.decode("utf-8"))
                        else:
                            line = self.process.stdout.readline(content_length - len(buffer))    # type: ignore
                            buffer = buffer + line
                    else:
                        msg = buffer[0: content_length]
                        buffer = buffer[content_length:]
                        content_length = None
                        self.emit_message(msg.decode("utf-8"))
                if self.process.stderr:
                    print(self.process.stderr.read())
            print("LSP server '{}' exited with code {}".format(self.server_name, self.process.returncode))
            print(self.process.stdout.read())    # type: ignore
            if self.process.stderr:
                print(self.process.stderr.read())
        except:
            import traceback
            print(traceback.format_exc())
