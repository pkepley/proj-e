# C++ Solutions
Here are a few solutions in C++. They're not very good (:

## Running the Programs
### Using Make
If you have GNU `make` installed, you can use the included `makefile` to build /
uninstall the solutions. When built, binaries will be exported into the `bin`
subdirectory of this folder.

#### Build
To build with `make`, run the following command from this folder:
```
make all
```
(Or just run `make` without any arguments from this folder).

#### Uninstall
Similarly, you can use `make` to quickly delete all the binaries by running the
following command from this folder:
```
make uninstall
```


### Roll Your Own
You can (maybe) build the solutions using your favorite C++ compiler -- although
I have only tested with `gcc` on Ubuntu. 

Most solutions depend on `prime_tools.c` so you throw that in while compiling. 
