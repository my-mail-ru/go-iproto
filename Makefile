GO = go
GOSRC = $(shell find . -type f -name '*.go')
TEST_TIMEOUT = 10s
LINT_TIMEOUT = 5m

.PHONY: all
all: format deps test full-lint iprotogen

.PHONY: iprotogen
iprotogen: bin/iprotogen

bin/iprotogen: $(GOSRC) go.mod go.sum
	$(GO) build -o $@ ./cmd/iprotogen

.PHONY: deps
deps:
	$(GO) mod tidy -v && $(GO) mod verify

.PHONY: lint
lint:
	$(info #Running lint...)
	golangci-lint run --timeout=$(LINT_TIMEOUT) --build-tags=golangci --new-from-rev=origin/$(shell (grep -s DEFAULT_BRANCH: .gitlab-ci.yml | cut -d: -f2 | sed 's/^\s*$$/master/' | tr -d ' \t'; echo master) | head -n1) ./...
#	golangci-lint run --timeout=$(LINT_TIMEOUT) --build-tags=golangci,integration --new-from-rev=origin/$(shell (grep -s DEFAULT_BRANCH: .gitlab-ci.yml | cut -d: -f2 | sed 's/^\s*$$/master/' | tr -d ' \t'; echo master) | head -n1) ./...

.PHONY: full-lint
full-lint:
	$(info #Running full lint...)
	golangci-lint run --build-tags=golangci ./...
#	golangci-lint run --build-tags=golangci,integration ./...

.PHONY: format
format:
	find . -name '*.go' | xargs $(GO) tool goimports -local github.com/my-mail-ru/ -l -w

.PHONY: test
test:
	$(GO) test $(GOFLAGS) -v     \
	    -timeout $(TEST_TIMEOUT) \
	    -race                    \
	    -covermode atomic        \
	    -coverprofile cover.out  \
	    -coverpkg ./...          \
	    ./...
	$(GO) tool cover -html cover.out -o cover.html

.PHONY: generate
generate:
	$(GO) generate ./...

.PHONY: doc
doc: API.md # doc/index.html

API.md: $(GOSRC)
	gomarkdoc --tags gomarkdoc --output $@ ./...

doc/index.html: $(GOSRC)
	godoc-static -destination=doc .
