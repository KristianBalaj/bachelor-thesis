# An alignment-free method with graphical representation for biological sequences comparison
Implementation of my bachelor thesis in the area of bioinformatics (An alignment-free method with graphical representation for genetic sequences comparison).

## Description
It generates points in the first and the second quadrant of the cartesian coordinate system. 
The points are generated from the biological sequence codons (triplets). 
Finally, a feature vector representing the sequence is created.
This feature vector is then used to compare different sequences with each other.
The result of the program is a `.tsv` formatted file where are the distances of every pair of biological sequeces from the input. 
The distance of a pair is a single decimal number.

## Usage
This command processes all the `.fasta` files in the `./data/assembled-ecoli` directory.

`bioinf-align-free-graphical --sequences-directory=./data/assembled-ecoli .fasta`


## Problems
In the end, this implementation couldn't be used in my thesis.
The reason was the **memory consumption**. 
There were many biological sequences in the benchmarking of my thesis. 
These sequences where in gigabytes of size altogether.
After some time of the program execution, the whole RAM and virtual memory of the OS was full and the program crashed.

The problem for me was the usage of the **laziness** of **Haskell**. 
This was **my first real program** written in **Haskell**.
I've tried to solve this with adding **strictness** to the code, but with no significant impact.
After some time of experimenting and profiling the code (which taught me a lot about **Haskell**), I've had to go with implementation in other language to finish my thesis, because there was no more time to experiment. 
In the other language, I was able to manage the memory consumption on the lower level and could work with more extensive data sets.

I really enjoyed coding in **Haskell**. I just need to gain more experience with for the better future 😀


