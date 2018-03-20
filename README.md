# erlpkg
```erlpkg``` is a small and simple Erlang utility which tries to help you build *'better'* EScript packages for your Erlang projects.

This project was originally written for [Jarlang](https://jarlang.org), which required more involved inclusion of files and directories into a resultant EScript, as well as argument parsing and other CLI niceties.

Ultimately, the main goal of ```erlpkg``` is to be a self contained, easy to use and feature filled EScript utilities package. It currently contains the following features:

- Build EScripts out of any number of files regardless of filetype
- EScript reflection: allowing the querying, modification and extraction of a given EScript even at runtime
- Simple to use CLI argument parser
- Simple to use CLI help text generator
- Erlpkg project boilerplate generator

For simple usecases though, you can build an EScript with ```erlpkg``` by running it and pointing it to a bunch of files; simplicity is at the core of the design.

## Getting Started
Please not that this project is still under active development. Things might be broken and interfaces may well change. Please do report any bugs and I'll do my best to fix them.

### Requirements
- Erlang 18 *(This is the only version I've tested on. I don't see much reason why later versions won't work. Earlier versions might be fine too as long as they support the Map datatype)*
- Unix-like environment *(Tested on ```Windows Subsystem for Linux```, ```Ubuntu``` and ```Arch Linux``` only)*

### Installing
1) Clone the repo with ```git clone https://github.com/vereis/erlpkg``` in your command line. This should create a folder in the working directory called ```erlpkg```.
2) Enter the ```erlpkg``` directory with ```cd erlpkg```
3) Run the ```make``` or ```make debug``` commands to build an erlpkg escript, with debug flags enabled or disabled respectively. The resultant files will be put in the ```ebin``` or ```edebug``` folders. 
4) *(Optional)* You can also run ```make test```, ```make dialyze``` or ```make lint``` to run the full test suite, dialyzer or linter respectively.

### Usage

Once you've built ```erlpkg```, you can build your first EScript by simple providing it a list of files (globbing supported) as arguments:

```shell
./erlpkg erlpkg.erl pkargs.erl
```

By default, this produces an EScript called ```erlpkg.erlpkg```. You can change the name of the resultant EScript as follows:

```shell
./erlpkg erlpkg.erl pkgargs.erl -o my_first_escript
```

EScript packages will start by running the ```main/1``` function of the entrypoint module which by default is the first Erlang source code file argument provided. You can set a custom entrypoint module with the following:

```shell
./erlpkg erlpkg.erl pkgargs.erl -o my_first_escript -e erlpkg.erl
```

The argument provided to the ```main/1``` function will be a list of arguments which you can either parse manually or you can parse with our built-in utility module ```pkgargs```. To see examples of ```pkgargs``` argument parsing at work, either check out ```src/erlpkg.erl``` or generate a basic project boilerplate with:

```shell
./erlpkg --gen-boilerplate
```

Help information is included, so if you need any more clarification, please do run:

```shell
./erlpkg -h
```

Otherwise, please do poke me for information and I'll do my best to get back to you as soon as possible.

### Advanced Use Cases

By default, ```erlpkg``` will automatically attach ```erlpkg```'s own utility modules, ```pkgargs``` and ```pkgutils``` into any EScript it builds.

As stated above, ```pkgargs``` is our argument parsing library which also comes with nice features such as generating help text for you automatically.

```pkgutils``` is our EScript reflection library, allowing us to do things such as query the contents of an EScript, add to an EScript or extract from an ESCript to an arbitary filesystem location.

You can, however, disable the automatic attaching of ```pkgargs``` and ```pkgutils``` by providing ```erlpkg``` with the command line option ```--no-utils```.

### Automatic Testing
Simply run ```make test``` in the project root directory.

You can also run ```make lint``` or ```make dialyze``` to run our linting or dialyzer steps without EUnit testing.

## Contribution Guidelines
Please ensure automatic testing passes, when implemented, before pushing any commits.

The ```vsn``` attribute is included seperately in each module. Please update these according to the scope of your changes:

- Update the first numeral if you're introducing a non-backwards compatible interface change to any given module *(renaming functions, for example)*
- Update the second numeral if you're introducing a backwards compatible interface change *(adding a new feature, for example)*
- Update the third numeral if you're changing code but you're not adding/removing any new features or breaking any interfaces *(simple bugfixes, for example)*