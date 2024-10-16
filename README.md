# Usage

- back up your `~/.config/doom/config.el` just in case
- clone the repo to a subdirectory in `~/.config/doom`
    - subsequent steps assume `~/.config/doom/shared`
- add the following line at the end of your `config.el`:
```elisp
(load! "shared/config.el")
```
- add the following line at the end of your `packages.el`:
```elisp
(load! "shared/packages.el")
```
- run `doom sync`

> Note:
> Any changes committed to source control should be platform and workstation agnostic.
