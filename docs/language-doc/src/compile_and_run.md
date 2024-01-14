# Compile and run your program

Now that we have our file `main.kop`, we'll have to compile it using GLaDOS

### Compile using GLaDOS

Compiling is pretty straight forward, all we need to do is to give our file to the GLaDOS program and use the `-c` argument

Let's compile our file `main.kop`:

```bash
> ./GLaDOS -c main.kop
```

Well done! Look, there is a new file in our directory:

```bash
> ls
main.wasm exectuable
```

### Run using GLaDOS

Now that you have your executable, you need to use the VM in the GLaDOS program.

To do so, use this command:

```bash
> ./GLaDOS -r executable
Hello world!
```
