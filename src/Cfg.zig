/// Memory size in bytes
mem_size: usize = 64 * 1024,

/// Number of cells in each stack
stack_size: usize = 128,

/// Maximum length of a line of input
max_line_len: usize = 80,

/// Which optional wordsets to include
optional_wordsets: struct {
    block: bool = false,
    double: bool = false,
    facility: bool = false,
    file: bool = false,
    float: bool = false,
    locals: bool = false,
    memory: bool = false,
    tools: bool = false,
    search: bool = false,
    string: bool = false,
    xchar: bool = false,
} = .{},
