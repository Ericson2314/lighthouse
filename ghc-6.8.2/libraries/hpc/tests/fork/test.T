setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci','threaded2']),
			    skip_if_fast)))

# Set up the hpc common variable(s).
hpc = config.top + '/../utils/hpc/hpc'

hpc_prefix = 'perl ../hpcrun.pl --clear --exeext=' + config.exeext + ' --hpc=' + hpc;

test('hpc_fork', cmd_prefix(hpc_prefix), compile_and_run, ['-fhpc'])

