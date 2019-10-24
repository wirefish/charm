#!/usr/bin/env python3

import sys
import os
import shutil
import subprocess

source_dir = sys.argv[1]
dest_dir = sys.argv[2]

for fn in os.listdir(source_dir):
    root, ext = os.path.splitext(fn)
    if ext == '.png' and not (root.endswith('_T') or root.endswith('_R')):
        dest_path = os.path.join(dest_dir, fn.lower())
        print(f'{fn} -> {dest_path}')
        if os.path.exists(dest_path):
            print(f'  *** will not overwrite existing file {dest_path}')
        else:
            shutil.copyfile(os.path.join(source_dir, fn), dest_path)
            subprocess.check_call(['/usr/bin/sips', '-Z', '128', dest_path],
                                  stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
