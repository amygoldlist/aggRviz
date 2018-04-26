# Contributing

`aggRviz` is a a new project, currently in development by our team (Susan Fung, Amy Goldlist and Fang Yang).  If you are interested in helping us out, we welcome suggestions!  Please read through the procedure below, check out our [code of conduct](conduct.md) and our [license](LICENSE).

## Raising issues

If you have any questions, concerns or suggestions, please [raise an issue](https://github.com/amygoldlist/aggRviz/issues).  

## Using Github

Fork our repo at :

*[https://github.com/goldlist/aggRviz.git](https://github.com/goldlist/aggRviz.git)*


Clone the repo:

```
git clone https://github.com/your_username/aggRviz.git
```

You can make changes in your local folder.


After you finish changes,

```
git commit
```

and

```
git push
```

the changes to your fork.

Then, [submit a pull request](https://github.com/goldlist/aggRviz/compare).

At this point you're waiting on us. We like to at least comment on pull requests within three business days (and, typically, one business day). We may suggest some changes or improvements or alternatives.


Some things that will increase the chance that your pull request is accepted:

 - Write tests.
 - Write good descriptive commit messages.
 - [enable _Travis CI_](https://travis-ci.org/) on your forked repo and ensure that your commits are Green lit.  Without passing _Travis_, we will not merge!
 - Note: The _Travis CI_ badge on the [ReadMe](README.md) is synced to the master repo at [amygoldlist/aggRviz](https://github.com/amygoldlist/aggRviz), and so will show as green even if your fork fails.  If you change the logo, please do not include this commit in your merge request!
 - If you make changes to [.travis.yml](.travis.yml) highlight the changes in your pull request. Please do not do this without good reason, instead, consider raising an [issue](https://github.com/amygoldlist/aggRviz/issues).

If the pull requests do not have any conflicts, at least one team members will reviews the pull requests before they can be merged.

## Attribution

This document is adapted from [factory-bot-rails/CONTRIBUTING.md](https://github.com/thoughtbot/factory_bot_rails/blob/master/CONTRIBUTING.md).
