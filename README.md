# erlpkg
Erlpkg is a small, simple and minimal Erlang utility to create standalone escripts for your Erlang projects.

This was originally written for [Jarlang](https://github.com/vereis/jarlang), which required other filetypes to be added into a resultant escript package, and thus ```erlpkg``` has the neat ability to encode arbitary filetypes and include them in generated escripts.

The main goal of ```erlpkg``` is ultimately to require no dependencies, and to allow you to build escripts containing anything you want as easily as running the erlpkg ```escript``` followed by a list of file arguments.

## Getting Started
Note that this project is still in development and very likely contains overlooked bugs and issues. Please be weary of this when considering this for real world usage. Please do report any issues you come across so that we can fix them.

### Requirements
- Erlang 18 (I assume other versions will work just fine, however, I've only tested this on a machine with Erlang 18 installed)

### Installing
1) Clone the repo with ```git clone https://github.com/vereis/erlpkg``` in your command line. This should create a folder in the working directory called ```erlpkg```.
2) Enter the ```erlpkg``` directory with ```cd erlpkg```
3) Run the ```make``` or ```make debug``` commands to build an erlpkg escript, with debug flags enabled or disabled respectively. The resultant files will be put in the ```ebin``` or ```edebug``` folders.

Once erlpkg is built, you can run it by simply providing it a list of files as arguments. Erlpkg will then automatically bundle them up into a valid escript for your use. An example of this might look like:
```shell
./erlpkg erlpkg.erl pkargs.erl
```
Which will produce an escript called ```erlpkg.erlpkg```. This is essentially how we build the escript for ```erlpkg``` itself.

Additional arguments can be added to change certain parameters, currently, the only arguments added are ```-e``` which determines which module is the main module of the generated escript, and ```-o``` to allow us to specify the name of the generated escript.

This updated example might look like:
```shell
./erlpkg pkgagrs.erl erlpkg.erl -e erlpkg -o erlpkg
```
This will create an escript named ```erlpkg``` instead of ```erlpkg.erlpkg``` and won't fail to run because even though ```pkgargs.erl``` is the first file added to the escript, we explicitly tell ```erlpkg``` to set the main entrypoint to the ```erlpkg``` module instead. 

More commands may be available so if you're curious run ```erlpkg -h``` for a help page.

Note: Any type of file can be included in an escript as escripts are essentially just fancy zip files, right now theres no way to easily and cleanly utilise other files added into an escript but one can easily play around with them by manually unzipping the contents of the escript. Additional erlpkg modules are going to be written and included in generated escripts by default for this purpose.

### Automatic Testing
We need to implement eunit testing, but other than that you can automatically run our linter, ```Elvis```, and our static analyser ```Dialyzer``` by running the command ```make test```.

## Contribution Guidelines
Please ensure automatic testing passes, when implemented, before pushing any commits. Also ensure you don't break the build.
