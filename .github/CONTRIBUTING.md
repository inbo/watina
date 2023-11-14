# Contributing to watina

First of all, thanks for considering contributing to watina! 👍 It's people like you that make it rewarding for us - the project maintainers - to work on watina. 😊

watina is an open source project, maintained by people who care. We are not directly funded to do so.

[repo]: https://github.com/inbo/watina
[issues]: https://github.com/inbo/watina/issues
[new_issue]: https://github.com/inbo/watina/issues/new
[website]: https://inbo.github.io/watina
[citation]: https://inbo.github.io/watina/authors.html
[email]: mailto:floris.vanderhaeghe@inbo.be

## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## How you can contribute

There are several ways you can contribute to this project. If you want to know more about why and how to contribute to open source projects like this one, see this [Open Source Guide](https://opensource.guide/how-to-contribute/).

### Share the love ❤️

Think watina is useful? Let others discover it, by telling them in person, via Twitter or a blog post.

Using watina for a paper you are writing? Consider [citing it][citation].

```r
# with the watina package installed, this will return a bibliography entry:
citation("watina")
```

### Ask a question ⁉️

Using watina and got stuck? Browse the [documentation][website] to see if you can find a solution. Still stuck? Post your question as an [issue on GitHub][new_issue]. While we cannot offer user support, we'll try to do our best to address it, as questions often lead to better documentation or the discovery of bugs.

Want to ask a question in private? Contact the package maintainer by [email][email].

### Propose an idea 💡

Have an idea for a new watina feature? Take a look at the [documentation][website] and [issue list][issues] to see if it isn't included or suggested yet. If not, suggest your idea as an [issue on GitHub][new_issue]. While we can't promise to implement your idea, it helps to:

* Explain in detail how it would work.
* Keep the scope as narrow as possible.

See below if you want to contribute code for your idea as well.

### Report a bug 🐛

Using watina and discovered a bug? That's annoying! Don't let others have the same experience and report it as an [issue on GitHub][new_issue] so we can fix it. A good bug report makes it easier for us to do so, so please include:

* Your operating system name and version (e.g. Mac OS 10.13.6).
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

Help yourself and those you are asking for help: use the reprex package!
Usually just running [`reprex::reprex(session_info = TRUE)`](https://reprex.tidyverse.org/reference/reprex.html) on a small reproducible example will do all the magic. 🌟

### Improve the documentation 📖

Noticed a typo on the website? Think a function could use a better example? Good documentation makes all the difference, so your help to improve it is very welcome!

#### The website

[This website][website] is generated with [`pkgdown`](http://pkgdown.r-lib.org/). That means we don't have to write any html: content is pulled together from documentation in the code, vignettes, [Markdown](https://guides.github.com/features/mastering-markdown/) files, the package `DESCRIPTION` and `_pkgdown.yml` settings. If you know your way around `pkgdown`, you can [propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to improve documentation. If not, [report an issue][new_issue] and we can point you in the right direction.

#### Function documentation

Functions are described as comments near their code and translated to documentation using [`roxygen2`](https://roxygen2.r-lib.org/). If you want to improve a function description:

1. Go to `R/` directory in the [code repository][repo].
2. Look for the file with the name of the function.
3. [Propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to update the function documentation in the roxygen comments (starting with `#'`).

### Contribute code 📝

Care to fix bugs or implement new functionality for watina? Awesome! 👏 Have a look at the [issue list][issues] and leave a comment on the things you want to work on. See also the development guidelines below.

## Development guidelines

### Coding tools

When writing functions for `watina`:

- please use `tidyverse`, `sf` and `raster` packages for data reading.
Discover the human-friendly way of coding a data processing pipeline through the use of [pipes](https://r4ds.had.co.nz/pipes.html)!
Organise data in R in a [tidy](https://r4ds.had.co.nz/tidy-data.html#tidy-data-1) way in order to avoid troubles later on.
Recommended resources to get started are:
    - [R for Data Science](https://r4ds.had.co.nz/)
    - [Geocomputation with R](https://geocompr.robinlovelace.net)
    - [R packages](https://r-pkgs.org/) (by Hadley Wickham and Jenny Bryan)
    - [`vignette("rd-formatting", package = "roxygen2")`](https://roxygen2.r-lib.org/articles/formatting.html) for documentation syntax.
    Or use markdown support in function documentation after adding the `@md` tag.
- have a quick look at the [tidyverse style guide](https://style.tidyverse.org/).
There you see how to style object, variable and function names, as well as the documentation.
At least keep in mind: **use lower case and 'snake_case'** for object, variable and function names.
- if your function returns a dataframe, use `dplyr::as_tibble()` to return it as a tibble instead.
A tibble is a dataframe that makes working in the tidyverse a little [easier](https://r4ds.had.co.nz/tibbles.html).
- functions that read or process data should return data as much as possible internationalized:
    - availability of English names for types, environmental pressures, ...
    Other languages can be accommodated as well;
    - English names for table headings (dataframe variables).


### How to contribute code? 

#### _Without_ write permissions to the [code repository][repo]

We try to follow the [GitHub flow](https://guides.github.com/introduction/flow/) for development.

1. Fork [this repo][repo] and clone it to your computer. To learn more about this process, see [this guide](https://guides.github.com/activities/forking/).
2. If you have forked and cloned the project before and it has been a while since you worked on it, [pull changes from the original repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/) to your clone by using `git pull upstream main`.
3. Open the RStudio project file (`.Rproj`).
5. Make your changes:
    * Write your code.
    * Test your code (bonus points for adding unit tests).
    * Document your code (see function documentation above).
    * Do an `R CMD check` using `devtools::check()` and aim for 0 errors and warnings.
5. Commit and push your changes.
6. Submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request).

#### _With_ write permissions to the [code repository][repo]

It is wise to first think about the scope of your function (or your proposed enhancement of an existing one), and talk it through with other collaborators:

- functions that are of broader interest than Natura 2000, better go to [inborutils](https://inbo.github.io/inborutils/) or a separate package;
- functions that will only be relevant to a specific _watina_ project, are better developed in the project-specific repo.

You will want to look at the file `src/manage_package.R` to get some useful packaging commands and developing tips.

**Releases, version numbering and the relation to git branches**

- We follow the semantic version numbering as described [here](https://r-pkgs.org/description.html#version).
- The commit referred to by `main` (branch tip) must always have a `<major>.<minor>.<patch>` version number in the `DESCRIPTION` file.
It is the latest released package version.
  - Subsequent commits on `main` which do not change the package code itself, but only website setup and repo documentation, must inherit the _same_ release version number.
  - **Commits which do change the package _must_ carry a development version number**; typically `<major>.<minor>.<patch>.9000`.
It follows that they never appear at the tip of the `main` branch.
Non-package commits _may_ follow this route as well: it is safe for all new commits.

  These conventions ensure that:
  - a simple package installation with `remotes::install_github()`, which defaults to downloading from the `main` branch, will result in an installation of the latest release;
  - the `pkgdown` [website] shows the version number of the latest release.
  
- Other branches than `main` can have various names.
However, there is always at least one **development branch** whose name begins with `dev`.
For example: `dev_nextrelease`, `dev_0.4.0`, ...
It is the collector of new features and bugfixes that will lead to a later release, and its first commit should be to add a dev-suffix (`.9000`) to the current version number (don't increment `<major>.<minor>.<patch>`).
  - Especially when cooperating, it is counteradvised to push directly to this branch; better do so through pull requests from feature branches.
- Eventually, the development branch's last commit before merging to `main` will be to increment at least one of `<major>`, `<minor>` or `<patch>` and to drop the dev-suffix from the version number (i.e. in the `DESCRIPTION` file).
Such final commits should happen directly on the development branch.
No later than that commmit (but it can safely be done earlier), also the `.zenodo.json` metadata file must be updated to the new release version number.


**Steps and tricks in git**

The preceding philosophy leads to following steps and guidelines:

1. Make commits (in your local clone of the remote repo on Github) _in your own git branch_, branched off from the **base** branch you wish to contribute to -- below referred as `<base>` branch.
Let's call your new branch the `<feature>` branch.
    - In general, the base branch will be a `dev*` branch, or it could be a feature branch of someone else you wish to make a contribution to.
    
    You can push your branch to the remote as often as you like, as it will not influence other branches (first time: do `git push -u origin yourbranchname`; afterwards `git push` suffices). It serves as a backup and enables others to work with you on that branch.
1. Meanwhile, make sure that your branch stays up to date with evolutions in the base branch.
This is needed to ensure a smooth merge of your branch to the base branch later on.
    - To do that in your local repo, you can run `git pull origin <base>` while having your feature branch checked out.
    - If you also wish to update your local base branch in this process, you can first `git checkout <base>` followed by `git pull`, then switch back to `git checkout <feature>` and merge the base branch with `git merge --no-ff <base>`.
    
    If any merge conflicts arise at this stage, resolve them in your own branch.
1. Propose to merge your commits into the base branch: after pushing your branch to GitHub (which you can do repeatedly), this starts with making a **pull request** (PR; actually this is a merge request) and assign at least one reviewer before a merge can be decided.
At that moment, open online discussion in the repo is possible on your changes (for other open discussion that you want to start, make an _issue_).
As long as no merge is performed, more commits can be added to this PR with `git push`, e.g. to implement requested changes by others.
    - make sure to correctly **set the base branch** in the pull request (because the default is `main`).
1. After your PR is merged, pull the base branch and clean up your local repo in order to keep up with the remote.

**Git resources**

- Info on general git workflows at INBO: <https://inbo.github.io/tutorials/tags/git/>.
See also [these git workshop materials](https://inbo.github.io/git-course/index.html).
- Günther T. (2014). Learn version control with Git: A step-by-step course for the complete beginner.
- <https://learngitbranching.js.org/>
- [Interactive Git cheatsheet](http://ndpsoftware.com/git-cheatsheet.html)





