# this helps get the order correct when adding new psa protocols and having
# to re-build the data objects. The protocols need to be built first
# because the other internal data objects depend on them.

PSA_CSVS=$(wildcard ./inst/lab_protocols/particle_size_analysis/*.csv)


all: ./R/sysdata.rda ./data/psa_protocols.rda ./data/psa_protocols_summary.rda

# these could likely be made with a pattern rule but
# I don't feel like working that out right now

# internal_data: ./R/sysdata.rda
./R/sysdata.rda: ./data-raw/internal_data.R ./data/psa_protocols.rda ./data/psa_protocols_summary.rda
	Rscript $<

./data/psa_protocols.rda: ./data-raw/psa_protocols.R $(PSA_CSVS)
	Rscript $<

./data/psa_protocols_summary.rda: ./data-raw/psa_protocols_summary.R $(PSA_CSVS)
	Rscript $<

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'devtools::check()'

build:
	Rcmd.exe INSTALL --no-multiarch --with-keep.source soiltestr

