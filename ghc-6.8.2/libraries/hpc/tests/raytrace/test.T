setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci']),
			    skip_if_fast)))

hpc = config.top + '/../utils/hpc/hpc';
hpc_prefix = 'perl ../hpcrun.pl --clear --exeext=' + config.exeext + ' --hpc=' + hpc, 'Main'

test('hpc_raytrace', \
	compose(skip_if_fast, cmd_prefix(hpc_prefix)), \
	multimod_compile_and_run, \
	['Main','-fhpc -package parsec'])

runCmd("cd " + getTestDir() + " && tar xf tixs.tar")

hpcsample = 'hpc_sample'

test('hpc_report_multi_001', normal, run_command, \
	[hpc + ' report ' + hpcsample + ' --include=Geometry --per-module'])
test('hpc_report_multi_002', normal, run_command, \
	[hpc + ' report ' + hpcsample + ' --exclude=Geometry --per-module'])
test('hpc_report_multi_003', normal, run_command, \
	[hpc + ' report ' + hpcsample + ' --include=Geometry --per-module --decl-list']) 

test('hpc_markup_multi_001', normal, run_command, \
	[hpc + ' markup ' + hpcsample + ' --include=Geometry'])
test('hpc_markup_multi_002', normal, run_command, \
	[hpc + ' markup ' + hpcsample + ' --exclude=Geometry'])
test('hpc_markup_multi_003', normal, run_command, \
	[hpc + ' markup ' + hpcsample + ' --fun-entry-count']) 

test('hpc_show_multi_001', normal, run_command, [hpc + ' show ' + hpcsample + ''])
test('hpc_show_multi_002', normal, run_command, [hpc + ' show ' + hpcsample + ' --include=Geometry'])

clean_o_hi()
