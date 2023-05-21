#! /bin/python3

# SuperFastPython.com
# load many files concurrently with asyncio
import os
from os import listdir
from os.path import join
import pathlib
import asyncio
import aiofiles
import tqdm
import tqdm.asyncio
import argparse

# open a file and return the contents
async def convert_file(filepath, semaphore):
    base,ext=os.path.splitext(filepath)
    targetpath = base + '.mp3'
    command = f'ffmpeg -i \"{filepath}\" -qscale:a 0 \"{targetpath}\"'
    # acquire the semaphore
    async with semaphore:
        if not os.path.isfile(targetpath):
            proc = await asyncio.create_subprocess_shell(
                command,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await proc.communicate()
        await os.remove(filepath)
                
        return filepath
        
# load all files in a directory into memory
async def main(path='tmp'):
    # prepare all of the paths
    paths = pathlib.Path(path).rglob("*.flac")
    # paths = [join(path, filepath) for filepath in listdir(path)]
    # create a semaphore to limit open files
    # semaphore = asyncio.Semaphore(100)
    semaphore = asyncio.Semaphore(3)
    file_log = tqdm.tqdm(total=0, position=1, bar_format='{desc}')
    # create coroutines
    tasks = [convert_file(filepath, semaphore) for filepath in paths]
    # execute tasks and process results as they are completed
    for task in tqdm.tqdm(asyncio.as_completed(tasks), total=len(tasks)):
        # open the file and load the data
        filepath = await task
        # file_log.set_description_str(f'Done: {filepath}')
        # report progress
        # print(f'.loaded {filepath}')
 
# entry point
if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        prog='tomp3',
        description='Convert sound files to mp3.')
    parser.add_argument('-m','--music',required=True)
    args = parser.parse_args()
    asyncio.run(main(args.music))
