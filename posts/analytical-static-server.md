This afternoon after my tea, I came up with an interesting idea for a static server.

The server itself should have as much RAM and disk space as possible (preferably HDDs).

it has a few components:

1. The file coordination/cache process
2. The statistics cache process
3. The scheduler process
4. The web interface
5. The C library for these processes (self explanatory)

### 1. The file coordination/cache process

You can think of this process as a data structure representing file storage (retrieval, caching, storage)

This is a process that can take a few commands:
- `store` - store a file on the disk
- `load` - load a file into memory
- `get` - get a file, load it into memory if not in memory

This process takes statistics of what it loads and sends them to the next process.

### 2. The statistics cache process

This process interfaces with a memory database, preferably one that can also persist to disk. It is responsible for recording information such as:

1. What times a file was accessed
2. Number of times a file was accessed each day

and calculating information such as:

1. What time of day (timeframe) a file should be in the cache
2. File priority (used internally for order of loading files)
3. Popular files

What this does is it allows the server to be "smart" and know when to dump and reassess the cache situation, vacuum databases, clear old data, etc BECAUSE the data is available.

It could also potentially send data off to an analytics server

It takes the commands:

- `record_file` - records a time and identifying information about the file.
- `get_scheduled_files` - returns the files needing to be loaded into the cache.
- `get_popular_files` - returns identifying information about the popular files

### 3. Scheduler process

Carries out actions proscribed by the statistics processes (warming files and such). Polls periodically, but infrequently (say every 1 to 5 minutes)

### 4. Web Interface

Three endpoints:

- `GET /files/<filename>`
- `POST /files`
- `GET /files/popular`