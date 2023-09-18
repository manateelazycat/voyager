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
import socket
from subprocess import PIPE
from sys import stderr
from threading import Thread
import threading

from core.utils import *

DEFAULT_BUFFER_SIZE = 100000000  # we need make buffer size big enough, avoid pipe hang by big data response from DAP server

class DapServer:

    def __init__(self):
        self.initialize_id = generate_request_id()

        self.dap_subprocess = subprocess.Popen(["python", "-m" "debugpy", "--listen", "5678", "--log-to", "/home/andy/debugpy_log", "--wait-for-client", "/home/andy/test.py"],
                                               bufsize=DEFAULT_BUFFER_SIZE,
                                               stdin=PIPE,
                                               stdout=PIPE,
                                               stderr=stderr)

        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_address = ('localhost', 5678)

        while True:
            try:
                self.socket.connect(server_address)
                print('connected')
                break
            except socket.error as e:
                import time
                print('connection failed, retrying...')
                time.sleep(0.3)  # 等待一段时间再尝试重新连接，这里设置为300毫秒

        self.receiver = DapServerReceiver(self.dap_subprocess, self.socket)
        self.receiver.start()

        self.sender = DapServerSender(self.dap_subprocess, self.socket)
        self.sender.start()

        self.receive_message_thread = threading.Thread(target=self.dap_message_dispatcher)
        self.receive_message_thread.start()

        self.send_initialize_request()

    def send_initialize_request(self):
        self.sender.send_request("initialize", {
            "clientID": "vscode",
            "clientName": "Visual Studio Code",
            "adapterID": "debugpy",
            "locale": "en-us",
            "linesStartAt1": True,
            "columnsStartAt1": True,
            "pathFormat": "path",
            "supportsVariableType": True,
            "supportsVariablePaging": True,
            "supportsRunInTerminalRequest": True,
            "supportsMemoryReferences": True,
            "supportsProgressReporting": True,
            "supportsInvalidatedEvent": True,
            "supportsMemoryEvent": True,
            "supportsArgsCanBeInterpretedByShell": True,
            "supportsStartDebuggingRequest": True
        }, self.initialize_id, init=True)

    def dap_message_dispatcher(self):
        try:
            while True:
                message = self.receiver.get_message()
                self.handle_recv_message(message)
        except:
            import traceback
            print(traceback.format_exc())

    def handle_recv_message(self, message: dict):
        print("!!!!! ", message)

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
    def __init__(self, process: subprocess.Popen, socket):
        super().__init__(process)

        self.socket = socket

        self.init_queue = queue.Queue()
        self.initialized = threading.Event()

    def enqueue_message(self, message: dict, *, init=False):
        if init:
            self.init_queue.put(message)
        else:
            self.queue.put(message)

    def send_request(self, command, arguments, request_id, **kwargs):
        self.enqueue_message(dict(
            seq=request_id,
            command=command,
            arguments=arguments,
            type="request"
        ), **kwargs)

    def send_notification(self, method, params, **kwargs):
        self.enqueue_message(dict(
            method=method,
            params=params,
            type="notification"
        ), **kwargs)

    def send_response(self, request_id, result, **kwargs):
        self.enqueue_message(dict(
            id=request_id,
            result=result,
            type="response"
        ), **kwargs)

    def send_message(self, message: dict):
        json_content = json.dumps(message)

        message_str = "Content-Length: {}\r\n\r\n{}".format(len(json_content), json_content)

        # self.process.stdin.write(message_str.encode("utf-8"))    # type: ignore
        # self.process.stdin.flush()    # type: ignore

        self.socket.sendall(message_str.encode("utf-8"))

        message_type = message.get("type")

        if message_type == "request":
            print("Send {} request ({})".format(
                message.get('command', 'response'),
                message.get('id', 'notification')
            ))
        elif message_type == "notification":
            print("Send {} notification".format(
                message.get('command', 'response')
            ))
        elif message_type == "response":
            print("Send response to server request {}".format(
                message.get('id', 'notification')
            ))

        print(json.dumps(message, indent=3))

    def run(self) -> None:
        try:
            # Send "initialize" request.
            self.send_message(self.init_queue.get())

            # Wait until initialized.
            self.initialized.wait()

            # Send other initialization-related messages.
            while not self.init_queue.empty():
                message = self.init_queue.get()
                self.send_message(message)

            # Send all others.
            while self.process.poll() is None:
                message = self.queue.get()
                self.send_message(message)
        except:
            import traceback
            print(traceback.format_exc())

class DapServerReceiver(MessageReceiver):

    def __init__(self, process: subprocess.Popen, socket):
        super().__init__(process)

        self.socket = socket

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
        while True:
            response = self.socket.recv(DEFAULT_BUFFER_SIZE)
            print("### Receive ", response)
        # try:
        #     content_length = None
        #     buffer = bytearray()
        #     while self.process.poll() is None:
        #         if content_length is None:
        #             match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
        #             if match is not None:
        #                 end = match.end()
        #                 parts = match.group(0).decode("utf-8").strip().split(": ")
        #                 content_length = int(parts[1])

        #                 buffer = buffer[end:]
        #             else:
        #                 line = self.process.stdout.readline()    # type: ignore
        #                 # dart_analysis_server 会发送 Content-Type,
        #                 # 导致解析的 json 回包内容不完整
        #                 if re.search(b"Content-Type", line) is None:
        #                     buffer = buffer + line
        #         else:
        #             if len(buffer) < content_length:
        #                 # 这个检查算是个防御吧，实际应该用不到了。先保留，后续再看。
        #                 match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
        #                 if match is not None:
        #                     start = match.start()
        #                     msg = buffer[0:start]
        #                     buffer = buffer[start:]
        #                     content_length = None
        #                     self.emit_message(msg.decode("utf-8"))
        #                 else:
        #                     line = self.process.stdout.readline(content_length - len(buffer))    # type: ignore
        #                     buffer = buffer + line
        #             else:
        #                 msg = buffer[0: content_length]
        #                 buffer = buffer[content_length:]
        #                 content_length = None
        #                 self.emit_message(msg.decode("utf-8"))
        #         if self.process.stderr:
        #             print(self.process.stderr.read())
        #     print(self.process.stdout.read())    # type: ignore
        #     if self.process.stderr:
        #         print(self.process.stderr.read())
        # except:
        #     import traceback
        #     print(traceback.format_exc())
