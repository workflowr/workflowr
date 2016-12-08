# Code

The files here are scripts that are run from the command line. It is often
useful for these scripts to take arguments as input. Several examples of how to
read command line arguments are provided, specifically in R, Python, and Bash. For practice, you can pass arguments to these functions.

```
$ code/script.R arg1 arg2 arg3
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

To be able to use these scripts from anywhere, first make them executable, e.g.

```
$ chmod +x script.R
```

Next open your `.bashrc` file and add the following line to include the scripts in `code` in the `PATH`. The example below demonstrates how to do this for a project called `example` in the home directory.

```
export PATH=$PATH:~/example/code
```

If you do this, you no longer need to provide the path to the executable file.

```
$ script.R arg1 arg2 arg3
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```
