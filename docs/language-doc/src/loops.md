# Loops

Loops are used to iterate based on a condition

## "for" loops

```
for (variable (optional); condition; action) {
	...
}
```

You can create a `variable` as a first argument but it's optional. To do so, use the same syntax to just create a variable:
```
for (var foo = 0; condition; action) {
	...
}
```

The `condition` is what is gonna determine the end of the `for` loop. As long as the condition is true, it's gonna keep looping
```
for (var foo = 0; foo < 10; action) {
	...
}
```

The `action` is what happens at every end of loop
```
for (var foo = 0; foo < 10; foo = foo + 1) {
	...
}
```

## "while" loops


```
while (condition) {
	...
}
```

The loop is gonna keep looping as long as the condition is true

## Example

#### **`main.???`**
```
int main(argc, argv)
{
	var foo = 42;

	for (foo = 0; foo < 10; foo = foo + 1) {
		print(foo);
	}

	print("\n")

	while (foo >= 0) {
		print(foo)
		foo = foo - 1
	}

	print("\nfoo:")
	print(foo)
}
```

To learn how to compile the file, [check this out!](./compile_and_run.md)

```
> ./GLaDOS -r executable
0123456789
9876543210
foo: -1
```
