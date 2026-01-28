.DEFAULT_GOAL := help

## Bootstrap project
bootstrap:
	pre-commit install --hook-type commit-msg --hook-type pre-commit

## Run tests
test:
	GUILE_AUTO_COMPILE=0 guile -s tests/run-tests.scm $(TEST)

## Run pre-commit hooks on all files
pre-commit:
	pre-commit run --all-files

## Check commit messages
check-commit:
	cz check --commit-msg-file "$$(git rev-parse --git-path COMMIT_EDITMSG)"

## Clean up
clean:
	@echo "Nothing to do yet"

## Show targets
help:
	@awk 'BEGIN{tabstop=8;targetcol=32} /^##/{desc=$$0;sub(/^##[ ]*/,"",desc);next} /^[a-zA-Z0-9_-]+:/{name=$$1;sub(/:.*/,"",name);col=length(name);pos=col;ntabs=0;while(pos<targetcol){ntabs++;pos=int(pos/tabstop+1)*tabstop}printf "%s",name;for(i=0;i<ntabs;i++)printf "\t";printf "%s\n",desc;desc=""}' Makefile
