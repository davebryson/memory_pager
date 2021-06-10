# Memory Pager

A utility to create small memory buffers for an application without needing to allocate
one big memory buffer. For example, maybe you need to reference data by bytes at random
locations but you don't know which locations use. Or, you're processing
chunks that may live at different byte positions. Rather than trying to create 1 large
buffer to handle this, you can use memory_page to allocate only the buffers needed.