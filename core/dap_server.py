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

class DapServer(Thread):

    def __init__(self):
        super().__init__()

    def run(self) -> None:
        self.initialize_id = generate_request_id()

        self.server_port = get_free_port()

        self.dap_subprocess = subprocess.Popen(
            ["python", "-m" "debugpy", "--listen", str(self.server_port),
             "--log-to", "/home/andy/debugpy_log",
             "--wait-for-client", "/home/andy/test.py"],
            bufsize=DEFAULT_BUFFER_SIZE,
            stdin=PIPE,
            stdout=PIPE,
            stderr=stderr)

        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_address = ('localhost', self.server_port)

        while True:
            try:
                self.socket.connect(server_address)
                print('Connected')
                break
            except socket.error as e:
                import time
                time.sleep(0.3)

        self.receiver = DapServerReceiver(self.socket)
        self.receiver.start()

        self.sender = DapServerSender(self.socket)
        self.sender.start()

        self.receive_message_thread = threading.Thread(target=self.dap_message_dispatcher)
        self.receive_message_thread.start()

        self.send_initialize_request()

    def send_initialize_request(self):
        self.sender.send_request("initialize", {
            "clientID": "emacs",
            "clientName": "Emacs",
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
        print("Receive: {}".format(json.dumps(message, indent=3)))

        if "command" in message and message["command"] == "initialize" and message["request_seq"] == self.initialize_id:
            self.sender.initialized.set()

            self.sender.send_request("attach", {
                "__restart": "restart"
            }, generate_request_id())

    def set_function_breakpoint(self, function_name):
        self.sender.send_request("setFunctionBreakpoints", {
            "breakpoints": [{
                "name": function_name,
                "condition": "",
                "hitCondition": ""
            }]
        }, generate_request_id())

    def configure_done(self):
        self.sender.send_request("configurationDone", {}, generate_request_id())

class DapServerSender(Thread):
    def __init__(self, socket):
        super().__init__()

        self.socket = socket

        self.queue = queue.Queue()
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

    def send_message(self, message: dict):
        json_content = json.dumps(message)

        message_str = "Content-Length: {}\r\n\r\n{}".format(len(json_content), json_content)

        self.socket.sendall(message_str.encode("utf-8"))

        message_type = message.get("type")

        if message_type == "request":
            print("Send {} request".format(message.get('command', 'response')))

        print(json.dumps(message, indent=3))

    def run(self) -> None:
        try:
            # Send "initialize" request.
            self.send_message(self.init_queue.get())

            # Wait until initialized.
            self.initialized.wait()

            # Send all others.
            while True:
                message = self.queue.get()
                self.send_message(message)
        except:
            import traceback
            print(traceback.format_exc())

class DapServerReceiver(Thread):

    def __init__(self, socket):
        super().__init__()

        self.socket = socket
        self.queue = queue.Queue()

    def get_message(self):
        return self.queue.get(block=True)

    def emit_message(self, line):
        if not line:
            return
        try:
            # Send message.
            self.queue.put(parse_json_content(line))
        except:
            import traceback
            print(traceback.format_exc())

    def run(self):
        while True:
            message = self.socket.recv(DEFAULT_BUFFER_SIZE)

            message_str = message.decode('utf-8')

            match = re.search(r'Content-Length: (\d+)', message_str)
            if match:
                content_length = int(match.group(1))
            else:
                print("No Content-Length header found.")
                content_length = None

            if content_length is not None:
                content_start = message_str.index('\r\n\r\n') + 4  # Start of content
                json_content = message_str[content_start:content_start+content_length]

                self.emit_message(json_content)
