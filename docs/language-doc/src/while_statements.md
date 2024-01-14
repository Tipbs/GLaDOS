# While Statements

Usage of while statements is pretty straight forward and easy to use.

```
while (condition) {
	...
};
```

If you'd like to see a real example

#### **`main.kop`**
```
fn main(argc, argv)
{
    var i = 0
    print("Here are all digit: ")
	While (i < 10) {
		print(i);
		print(" ");
	};
```

To learn how to compile the file, [check this out!](./compile_and_run.md)

```sh
> ./GLaDOS -r executable
Here are all digit: 1 2 3 4 5 6 7 8 9 
```
