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
import queue
import threading
import traceback
import sys
from pathlib import Path
from epc.server import ThreadingEPCServer
from core.dap_server import DapServer
from utils import (init_epc_client, eval_in_emacs, logger, close_epc_client, message_emacs)

class Voyager:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        self.server_dict = {}

        # Build EPC server.
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(voyager_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()
        
        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # Pass epc port and webengine codec information to Emacs when first start voyager.
        eval_in_emacs('voyager--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                message = self.event_queue.get(True)
                print("**** ", message)
                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def start(self, path):
        self.start_server(path)

    def start_server(self, path):
        if path not in self.server_dict:
            server_thread = DapServer()
            self.server_dict[path] = server_thread
            server_thread.start()

    def set_function_breakpoint(self, path, function_name):
        if path not in self.server_dict:
            message_emacs("Please execute voyager_start first.")
        else:
            self.server_dict[path].set_function_breakpoint(function_name)

    def configure_done(self, path):
        if path not in self.server_dict:
            message_emacs("Please execute voyager_start first.")
        else:
            self.server_dict[path].configure_done()

    def cleanup(self):
        """Do some cleanup before exit python process."""
        self.server_dict = {}
        close_epc_client()

if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("Voyager(sys.argv[1:])")
    else:
        Voyager(sys.argv[1:])
