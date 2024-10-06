# mdgen

`mdgen` is a tool to generate `.md` files from `.lean` files.

A similar tool, [lean2md](https://github.com/arthurpaulino/lean2md), is already available, but it is written in Python. `mdgen` is written purely in Lean. And mdgen has more features!

## How to use

Add this repository to your `lakefile`:

```lean
require mdgen from git
  "https://github.com/Seasawher/mdgen" @ "main"
```

Don't forget to run `lake update mdgen` after editing the `lakefile`. And simply run `lake exe mdgen <input_dir> <output_dir>`.

## Features

`mdgen` has the following features:

* The comments enclosed with an `/-! ... -/` or `/- ... -/` are converted as ground text.

* Nested block comments can also be handled. You can also insert a code block in the block comment.

* The inline comment, doc comment and non-comment parts are converted to lean code blocks.

* Lines ending with `--#` are ignored.

* Lines enclosed by `--#--` are ignored.

* Directories within `input_dir` will also be converted.

* Uniform internal link syntax is supported. You can write the path from the `output_dir` with the symbol `#{root}` in a markdown part. mdgen will automatically insert the required number of `../`. Thus links to the same file can be written in the same way regardless of where they are referenced from.

If you want to know more details, check the test code.

* [source](./Test/Src/First.lean)
* [expected output](./Test/Exp/First.md)

## Acknowledgments

I would like to acknowledge the author of [lean2md](https://github.com/arthurpaulino/lean2md), [@arthurpaulino](https://github.com/arthurpaulino).
