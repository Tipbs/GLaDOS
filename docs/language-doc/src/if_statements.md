# If Statements

Usage of if statements is pretty straight forward and easy to use.

```
if (condition) {
	...
} else {
	...
}
```

If you'd like to see a real example

#### **`main.kop`**
```
fn main(argc, argv)
{
	if (4 > 9) {
		print("It is true!")
	} else {
		print("It is false!")
	}
}
```

To learn how to compile the file, [check this out!](./compile_and_run.md)

```sh
> ./GLaDOS -r executable
It is false!
```
