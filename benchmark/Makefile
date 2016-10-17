iterations=10
sigdir=./sig
sigfile_orig=$(sigdir)/orig.xml
sigfiles=$(sigdir)/pdfa.xml \
		 $(sigdir)/pdf.xml \
		 $(sigdir)/tiff.xml \
		 $(sigdir)/wave.xml \
		 $(sigdir)/word-excel.xml \
		 $(sigdir)/xml.xml
datadir=./data/
result=./benchmark.csv

$(result): $(sigfiles)
	@echo -n 'Start benchmarking ... '
	@./util/benchmark.pl \
		-i $(iterations) \
		-d $(datadir) \
		$(sigfile_orig) $(sigfiles) \
		> $@
	@echo 'Done.'
	@echo 'Please find the benchmark results in the $(result) file!'

$(sigdir)/pdfa.xml: $(sigfile_orig)
	@echo -n 'Filter signature file for PDF/A ... '
	@./util/filter.sh $< 'PDF/A' > $@
	@echo 'Done.'

$(sigdir)/pdf.xml: $(sigfile_orig)
	@echo -n 'Filter signature file for PDF ... '
	@./util/filter.sh $< 'Acrobat PDF 1\.[0-9]' > $@
	@echo 'Done.'

$(sigdir)/wave.xml: $(sigfile_orig)
	@echo -n 'Filter signature file for WAVE ... '
	@./util/filter.sh $< 'Waveform Audio|Broadcast WAVE' > $@
	@echo 'Done.'

$(sigdir)/word-excel.xml: $(sigfile_orig)
	@echo -n 'Filter signature file for Word/Excel ... '
	@./util/filter.sh $< 'Microsoft (Word|Excel)' > $@
	@echo 'Done.'

$(sigdir)/xml.xml: $(sigfile_orig)
	@echo -n 'Filter signature file for XML ... '
	@./util/filter.sh $< '^(fmt/101|x-fmt/28[01])\>' > $@
	@echo 'Done.'

$(sigdir)/tiff.xml: $(sigfile_orig)
	@echo -n 'Filter signature file for TIFF ... '
	@./util/filter.sh $< 'Tagged Image File Format' > $@
	@echo 'Done.'

$(sigfile_orig):
	@echo -n 'No signature file found, download current version ... '
	@mkdir -p $(sigdir)
	@./util/download.sh > $@
	@echo 'Done.'

clean:
	@rm -rf $(sigdir)
	@rm -f $(result)

