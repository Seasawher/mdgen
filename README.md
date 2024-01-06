# mdgen

`mdgen` is a tool to generate `.md` files from `.lean` files. A similar tool, [lean2md](https://github.com/arthurpaulino/lean2md), is already available, but it is written in Python. `mdgen` is written purely in Lean.

## How to use

Add this repository to your `lakefile`:

```lean
require mdgen from git
  "https://github.com/Seasawher/mdgen" @ "main"
```

Don't forget to run `lake update mdgen` after editing the `lakefile`. And simply run `lake exe mdgen <input_dir> <output_dir>`.

## Features

`mdgen` has the following features:

* The comments enclosed with an `/-! ... -/` are converted as ground text.

* The non-comment parts of the `.lean` file are converted to lean code blocks.

* Lines ending with `--#` are ignored.

* Directories within `input_dir` will also be converted.

If you want to know more details, check the test code. Files under the `Test/Src` directory will look like files under `Test/Exp` when converted.

## Acknowledgments

Many parts of the code are copied from [lean2md](https://github.com/arthurpaulino/lean2md). Thank you [@arthurpaulino](https://github.com/arthurpaulino).
