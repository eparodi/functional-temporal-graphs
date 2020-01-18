# Functional Temporal Graph

This project implements a library for graph functions and a web GUI to do some data exploration of temporal graphs done with Happstack and Blaze.

The library contains an implementation for:

- Simple Graph   (Unweighted edges)
- Weighted Graph (Weighted edges)
- Temporal Graph (Temporal weighted edges)

## Requirements

- Haskell
- Stack
- Cabal
- Linux (To run Happstack)

## How to run the GUI

- Run the following command:

```
stack run
```

- Open your browser and go to `http://localhost:8000`.
- Start using it!

## CSV format

The CSV file to run the algorithms must follow some rules:

- It must not contain a header.
- It must contains 4 columns.
    - Source Node.
    - Destination Node.
    - Departure Time.
    - Duration.