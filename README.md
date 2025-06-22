# Advent of Code

[Advent of Code](https://adventofcode.com/) solutions.


## Data

Puzzle inputs are not shared publicly, but you can download your own using the provided script.

### How to Run

1. Get your session token:
    - Log in to [Advent of Code](https://adventofcode.com/)
    - Open your browser's developer tools and go to the **Application** (Chrome) or **Storage** (Firefox) tab
    - Locate the `session` cookie under **Cookies**
    - Copy its value and save it to a file named `session.txt` inside the `data` directory (this file is git ignored)

2. (optional) Make the script in the `data` directory executable:
```sh
chmod +x ./fetch.py
```

3. Run the script from the `data` directory:
```sh
./fetch.py [year]
```

If no `year` is given, the current year is used.

### Structure

The script will download all available input files for the specified year, saving them in the `data` directory.
Existing files are not overwritten.
Files are stored in the format `data/{yyyy}/{dd}.txt`, where:
- `{yyyy}` represents the year (e.g., 2023)
- `{dd}` represents the day of the puzzle (e.g., 01 for December 1st)
