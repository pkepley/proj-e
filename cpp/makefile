##
# proj-e
#
# @file
# @version 0.1

CC= g++
SRCDIR := src
LIBDIR := lib
BINDIR := bin

# Hand curated list of "common" files
SRC_COMMON := prime_tools.cpp
SRC_COMMON := $(patsubst %.cpp, $(SRCDIR)/%.cpp, $(SRC_COMMON))
OBJS := $(patsubst $(SRCDIR)/%.cpp, $(LIBDIR)/%.o, $(SRC_COMMON))

# All the stand-alone source files for stand-alone programs
SRCS := $(wildcard $(SRCDIR)/*.cpp)
SRCS := $(filter-out $(SRC_COMMON), $(SRCS))
PROGS := $(patsubst $(SRCDIR)/%.cpp, $(BINDIR)/%, $(SRCS))

.PHONY: directory
.PHONY: uninstall

all: directory $(OBJS) $(PROGS)

directory:
	mkdir -p $(BINDIR)
	mkdir -p $(LIBDIR)

# compile common objects to link against
$(LIBDIR)/%.o: $(SRC_COMMON)
	$(CC) $< -c -o $@

# compile the individual programs
$(BINDIR)/%: $(SRCDIR)/%.cpp
#   WARNING: I am VERY LAZILY compiling with ALL the common
#   objects, regardless of whether they are used in a program...
#   this is because I DO NOT UNDERSTAND MAKE!
#   TODO: fix this!
	$(CC) $< -o $@ $(OBJS)

uninstall:
	rm -rf $(BINDIR)
	rm -rf $(LIBDIR)

# end
