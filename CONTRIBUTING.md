# Contributing

Thanks for your interest in contributing to workflowr.
Here are some guidelines to help make it easier to merge your Pull Request:

* For potentially large changes, please open an Issue first to discuss
* Please submit Pull Requests to the "dev" branch
* Please follow the [Hadley style guide][style]
* Run `devtools::test()` to run the tests
* Execute the file `document.R` to update the documentation

If you're new to submitting Pull Requests, please read the section [Contribute
to other projects][contribute] in the tutorial [A quick introduction to version
control with Git and GitHub][git-tutorial].

[style]: http://adv-r.had.co.nz/Style.html
[contribute]: http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1004668#sec011
[git-tutorial]: http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1004668

## Explanation of branches

branch name   | purpose
------------- | -------------
master        | stable branch for end users
dev           | development branch - submit Pull Requests here

## Release checklist

* Bump version in [DESCRIPTION](DESCRIPTION), [README.md](README.md), and
[NEWS.md](NEWS.md)
* Update [NEWS.md](NEWS.md): Check `git log` and make sure to reference GitHub
Issues/PRs
* Run [document.R](document.R) to update Rd files, install the package locally,
and build the online documentation
    * Need to install [pkgdown][] from previous [commit][pkgdown-commit] to
    avoid errors due to dashes in vignette names:
    `devtools::install_github("hadley/pkgdown@b305b79")`
* Run [build.sh](build.sh) to confirm tests pass
* Commit with `git commit -am "Bump version: x.x.x.9xxx -> x.x.x and re-build
docs."`
* Push with `git push origin dev` and wait for CI builds to pass
* Merge into master:
    ```
    git checkout master
    git merge dev
    git push origin master
    ```
* Tag with `git tag -a vx.x.x`. Summarize [NEWS.md](NEWS.md) entry into bullet
points. Run ` git tag -l -n9` for past examples. Push with `git push origin
--tags`.
* Make a release. On GitHub, go to Releases -> Tags -> Edit release notes. Name
the release "workflowr x.x.x" and copy-paste the Markdown entry from
[NEWS.md](NEWS.md).
* Bump version in [conda recipe][meta.yaml] (also add any new dependencies),
build for R 3.3.2 and 3.4.1 on Linux and macOS (with conda-build v2), and upload
to [Anaconda Cloud][anaconda]
    ```
    conda build --version
    cd jdb-conda/recipes
    conda build --R 3.3.2 r-workflowr
    conda build --R 3.4.1 r-workflowr
    ```

[anaconda]: https://anaconda.org/jdblischak/r-workflowr
[meta.yaml]: https://github.com/jdblischak/jdb-conda/blob/master/recipes/r-workflowr/meta.yaml
[pkgdown]: https://github.com/r-lib/pkgdown
[pkgdown-commit]: https://github.com/r-lib/pkgdown/issues/363#issuecomment-310187626
