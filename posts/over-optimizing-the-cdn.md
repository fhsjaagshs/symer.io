This afternoon, I was wondering how to more efficiently serve files. Files from CSS to PNGs to your grandma's recipes. So I dreamt up this idea after tea.

The idea is to rank files based on:
1. how often they're accessed
2. where they're accessed
3. when they're accessed

Using this ranking, we can determine when, where, and how to store files on a CDN.

For example (given n, the memory storage threshold):

- A file is accessed most frequently around 10:00am UTC in London
- It's accessed more often than n times

Send the file to the server closest to London around 9:30am UTC and store it in memory.

Contents:
1. High-Level I/O Operations
2. File Access statistics
3. Distributed System

### 1. High-Level I/O Operations

Since actually performing I/O is not the focus of the CDN, we'll abstract it into a few operations:

- `file` - store a file on the disk
- `memory` - store a file directly in memory, skipping the disk
- `persist` - write a file in memory to the disk
- `memorize` - load a file into memory
- `get` - get a file from either the disk or memory. Prefers memory.

### 2. File Access Statistics

This is the heart of this CDN implementation. The CDN tracks each file access; It records the time, which file, and IP address of the computer accessing a file. This information can be used to determine where to store the file.

### 3. Distributed System

CDNs are often unnecessarily shuffling around files, consuming bandwidth, energy, and time. They do this because they don't know any better. Furthermore, they blindly sync files across servers, incurring extra complexity from syncing algorithms. 

This approach is different: Rather than keeping every file everywhere, files get uploaded to individual servers that propagate the file as needed. When a file is uploaded, our CDN checks all available nodes for a file with the same name, replacing as needed. It's case sensitive.

This system is obviously represented by a graph. Each node is a server and the edges are associations between servers. When a file is needed from a specific server, it requests it from any associated servers. This process is recursive.

Each node is given a durability rating. Node selection is a function of durability and distance.

#### Node Types

From highest durability (#1) to lowest durability (#4)

1. **Glacier** - Acts as long-term storage for files: Any file uploaded to them is persisted to the disk only. Does not serve files and is not public facing.
2. **Hybrid** - Stores files in memory and on the disk. Where the file is stored depends on how often it's accessed.
3. **Mirror** - Mirrors another node, storing files in memory, disk, or both. Forwards all writes to the server it mirrors. If the node it's mirroring goes down, it is promoted to the node type of that node. That node is then flagged for reset.
4. **Sparse** - Stores files in memory only. They cannot persist files, and forward the storage action to the nearest capable node.