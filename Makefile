################################################################################
# Configuration
################################################################################

GHC = ghc
GHC_FLAGS = -O2 -fforce-recomp -ibenchmarks -i../blaze-html/benchmarks -i../blaze-html

BENCHMARK_FLAGS = --samples 5 --resamples 5

################################################################################
# Benchmarks
################################################################################

benchmark:
	# Check that blaze-html exists
	if [[ ! -d ../blaze-html ]]; then echo "../blaze-html not found"; exit 1; fi
	$(GHC) $(GHC_FLAGS) --make -main-is RunHtmlBenchmarks benchmarks/RunHtmlBenchmarks.hs
	./benchmarks/RunHtmlBenchmarks $(BENCHMARK_FLAGS)
