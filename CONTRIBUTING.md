# Contributing

Thanks for your interest in contributing to workflowr.
Here are some guidelines to help make it easier to merge your Pull Request:

* For potentially large changes, please open an Issue first to discuss
* Please submit Pull Requests to the "dev" branch
* Please follow the [Hadley style guide][style]
* Run `devtools::test()` to run the tests

If you're new to submitting Pull Requests, please read the section [Contribute
to Other Projects][contribute] in the tutorial [A Quick Introduction to Version
Control with Git and GitHub][git-tutorial].

[style]: http://adv-r.had.co.nz/Style.html
[contribute]: http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1004668#sec011
[git-tutorial]: http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1004668

## Explanation of branches

branch name   | purpose
------------- | -------------
master        | stable branch for end users
dev           | development branch - submit Pull Requests here
gh-pages      | Hosts the vignettes online (in `inst/doc/`)
