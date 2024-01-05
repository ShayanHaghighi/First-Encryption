# First-Encryption

This was a fun project I did to test out a simple way of encrypting/decrypting text.

## Prerequisites

- [Haskell Platform](https://www.haskell.org/platform/): Make sure Haskell is installed on your machine.

## Getting Started

1. Clone the repository:

    ```bash
    git clone https://github.com/ShayanHaghighi/Encryption-Example.git
    ```

2. Navigate to the project directory:

    ```bash
    cd Encryption-Example
    ```

3. Build the project using Cabal:

    ```bash
    cabal build
    ```

## Running the Program

To run the program, use the following command:

```bash
cabal run enc <string to encrypt>
```
to encrypt a string and

```bash
cabal run dec <string to decrypt>
```
to decrypt a string.

**Note:** The command may take longer on the first it is run as the cabal manager needs to resolve the program's dependencies.
