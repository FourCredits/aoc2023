# Advent of Code 2023

## TODO

- [x] Review day 1
    - [x] better names
- [x] better module organisation
    - [x] proper versioning of dependencies
- [x] unit tests
    - [x] day 1
    - [x] day 2
- [x] make api of modules better something like `String -> (String, String)` so you can skip parsing twice
- [x] publish
- [x] rename `src` to `lib`
- [x] use the `Paths_` and data files to store the input files
- [x] use `Text` rather than `String`?
- [x] experiment with using the `relude`?
- [x] format
- [x] fix failing test for day 2
- [x] clean-up day 3
- [ ] extract out common threads (e.g. int parser)
- [x] optimise and tidy day 7, especially part 2
- [ ] use `context` more often in tests, compared to how much i use `describe`
- [ ] make tests run in parallel
- [ ] use `focus` from hspec to control what gets run
- [ ] optimise day 14 part 2 to run a little quicker
- [ ] use `#` across the whole library
- [ ] optimise day 16 part 2 - probably involves repeating less work
- [ ] find a way to do day 20 part 2 programmatically (more below)

## Day 20 part 2

i've currently left day 20 part 2 unimplemented. or at least, unimplemented in code. i had a solution, but it would have taken an impractical amount of time to complete, so i abandoned it. what i instead was looked at the input really closely. you can arrange the dependencies between modules in such a way to reveal an interesting features - there are "subnetworks", where only one module talks to anything outside of the subnetwork. each subnetwork's one contact with the outside world is what goes to `rx`. the rest form a [linear-feedback shift register](https://en.wikipedia.org/wiki/Linear-feedback_shift_register) (look particularly at the "Uses as counters" section), which counts to a particular number (call it "x"), then triggers the subnetwork's output every x button presses. i had four subnetworks, each with a prime number period (3923, 4091, 4001, and 3847). they all sync up at the lowest common multiple of those periods - which, given that they're prime numbers, is the product of the number (247_023_644_760_071) - a rather large number! incidentally, multiplying large prime numbers together (although normally larger than this), is generally how hash functions work.

what i did was modify the input file to only contain one subnetwork at a time, and then ran my (interim) part 2 solution over it (which is how i got the periods mentioned above in the first place), and then found the product of the results. elegant? no. automatic? certainly not. but it did give me a correct answer. with that said, i would like to try and make a fully automatic version, that can split up the subnetworks as i outlined above, and work out what the answer should be.
