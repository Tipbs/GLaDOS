# Function that returns a value

As said on the [Hello world! page](./hello_world.md), functions can return value that can be used or stored in a variable.

#### **`main.kop`**
```
fn sum(x, y)
{
	return x + y;
}

fn main()
{
	var x = 5;
	var y = 8;
	var z = sum(x, y);

	print(z);
	print(sum(z, x));
}
```

To learn how to compile the file, [check this out!](./compile_and_run.md)

```
> ./GLaDOS -r executable
13
18
```
