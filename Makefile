.PHONY: test scripts purs format format-check

SBT ?= sbt

# Where `make scripts` writes the <Name>.script text envelopes
SCRIPTS_DIR ?= out/scripts

# Name and path of the PureScript module written by `make purs`
PURS_MODULE ?= Fida.RawScripts
PURS_FILE ?= out/purs/Fida/RawScripts.purs

# Run the test suite
test:
	$(SBT) test

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
