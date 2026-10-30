.PHONY: test coverage mutation scripts purs format format-check

SBT ?= sbt

# Where `make scripts` writes the <Name>.script text envelopes
SCRIPTS_DIR ?= out/scripts

# Name and path of the PureScript module written by `make purs`
PURS_MODULE ?= Fida.RawScripts
PURS_FILE ?= out/purs/Fida/RawScripts.purs

# Run the test suite
test:
	$(SBT) test

# Run the test suite measuring coverage; fails below 90% of statements or branches
# (report: target/scala-3.3.8/scoverage-report/index.html)
coverage:
	$(SBT) clean coverage test coverageAggregate

# Mutation testing of the contracts; fails unless every mutant is killed
# (report: target/stryker4s-report/<timestamp>/index.html)
mutation:
	$(SBT) mutationTest

# Write the unapplied contracts as cardano-api text envelopes to $(SCRIPTS_DIR)
scripts:
	$(SBT) "run -o $(SCRIPTS_DIR)"

# Write the PureScript module $(PURS_MODULE) embedding the contracts to $(PURS_FILE)
purs:
	$(SBT) "run -n $(PURS_MODULE) -p $(PURS_FILE)"

# Format the Scala sources and the sbt build files
format:
	$(SBT) scalafmtAll scalafmtSbt

# Check the formatting of the Scala sources and the sbt build files
format-check:
	$(SBT) scalafmtCheckAll scalafmtSbtCheck
