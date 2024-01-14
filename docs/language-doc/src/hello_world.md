# Hello world!

We'll start off by writing a simple hello world.

### But first, we'll learn how to create a function.

```
fn foobar(a, b)
{
	...
}
```
- `fn` tells the code that a function is being created.
- `foobar` is the name of the function, choose whatever you want.
- `(a, b)` are the function arguments, define them using the parenthesis, then give them a name (here `a` and `b`)
- `{ ... }` open the function, that's where your code will go

Functions can also return a value using the `return` keyword followed by a value (example: `return 4`).

### Alright, now that you know how to create a function, let's introduce the `main` function.

The `main` function is the function that is being executed first, so if we want to write a Hello world!, then we'll have to create a main function:

#### **`main.kop`**
```
fn main()
{
	print("Hello world!\n");
}
```
There you go, Hello world!

The `print` function takes 1 argument, here a string, that is gonna be written to the screen. The `\n` at the end of the string means that we want to add a new-line at after the text.

Now that we have created our `main` function printing "Hello world!" to the terminal, we need to compile and run our code, head up to the next part to learn how!
