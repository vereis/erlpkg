# erlpkg
Erlpkg is a small, simple and minimal Erlang utility to create standalone escripts for your Erlang projects.

This was originally written for [Jarlang](https://github.com/vereis/jarlang), which required other filetypes to be added into a resultant escript package, and thus ```erlpkg``` has the neat ability to encode arbitary filetypes and include them in generated escripts.

The main goal of ```erlpkg``` is ultimately to require no dependencies, and to allow you to build escripts containing anything you want as easily as running the erlpkg ```escript``` followed by a list of file arguments.

## Getting Started
Note that this project is still in development and very likely contains overlooked bugs and issues. Please be weary of this when considering this for real world usage. Please do report any issues you come across so that we can fix them.

### Requirements
- Erlang 18 (I assume other versions will work just fine, however, I've only tested this on a machine with Erlang 18 installed)
- Some form of Linux? I primarily develop with ```Windows Subsystem for Linux``` running ```Ubuntu```. I've not tested anywhere else.

### Installing
1) Clone the repo with ```git clone https://github.com/vereis/erlpkg``` in your command line. This should create a folder in the working directory called ```erlpkg```.
2) Enter the ```erlpkg``` directory with ```cd erlpkg```
3) Run the ```make``` or ```make debug``` commands to build an erlpkg escript, with debug flags enabled or disabled respectively. The resultant files will be put in the ```ebin``` or ```edebug``` folders.

Once erlpkg is built, you can run it by simply providing it a list of files as arguments. Erlpkg will then automatically bundle them up into a valid escript for your use. An example of this might look like:
```shell
./erlpkg erlpkg.erl pkargs.erl
```
Which will produce an escript called ```erlpkg.erlpkg```. This is essentially how we build the escript for ```erlpkg``` itself.

## Usage
```erlpkg``` attempts to be as easy to use as possible, but it also contains slightly more advanced functions for more complex use-cases:

### General Use Case
You can build an erlpkg out of most types of files by simple running the ```erlpkg``` binary and passing in a list of files you want to include.

By default, the assumed escript entrypoint is the first file in this list. You can override with with the ```-e MODULE``` command line option. The default output of ```erlpkg``` will simply be whatever the escript entrypoint is with the file extension ```.erlpkg```.

An example of this usage might be:
```shell
./erlpkg fibonnaci.erl math.erl other_thing.beam main.erl -e main -o my_erl_pkg
```

### Advanced Use Cases
By default, ```erlpkg``` will automatically attach the modules ```pkgargs``` and ```pkgutils``` into anything it builds. This is to allow us to perform basic escript argument parsing in a nice way, generate ```--help``` messages automatically and poke at the contents of the escript if we need to.

```pkgargs``` is our argument parsing library. Unfortunately, we don't have any documentation outside of reading the source code (although its all highly commented) at the moment, but you can use that to parse command line arguments which is actually what ```erlpkg``` itself does.

```pkgutils``` also gives our packages the ability to list the files inside said packages, or even extract parts of the package to either an automatically generated temporary directory (usually ```/tmp/ERLPKG_NAME/```) or an arbitrary directory of your choice. This can be used for great effect if, say, like [Jarlang](https://github.com/vereis/jarlang), you need to extract some modules for use with ```NodeJS```. This is actually how ```erlpkg``` attaches these modules into generated packages!

You can, however, disable the automatic attaching of ```pkgargs``` and ```pkgutils``` by providing ```erlpkg``` with the command line option ```--no-utils```.

### Automatic Testing
Simply run ```make test``` in the project root directory.

## Contribution Guidelines
Please ensure automatic testing passes, when implemented, before pushing any commits. Also ensure you don't break the build.