# Function that returns a value

As said on the [Hello world! page](./hello_world.md), functions can return value that can be used or stored in a variable.

#### **`main.???`**
```
int sum(x, y)
{
	return x + y;
}

int main(argc, argv)
{
	var x = 5;
	var y = 8;
	var z = sum(x, y);

	print(z)
	print(z + sum(z, x))
}
```

To learn how to compile the file, [check this out!](./compile_and_run.md)

```
> ./GLaDOS -r executable
13
18
```
