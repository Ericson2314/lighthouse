setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci']),
			    skip_if_fast)))

# Set up the hpc common variable(s).
hpc = config.top + '/../utils/hpc/hpc'

hpc_prefix = 'perl ../hpcrun.pl --clear --exeext=' + config.exeext + ' --hpc=' + hpc;

test('hpc001', cmd_prefix(hpc_prefix), compile_and_run, ['-fhpc'])

# a prepackaged version of .hpc/*mix and hpc-sample.tix files.
# This is created using 'make build-tix-tar', in this directory.

runCmd("cd " + getTestDir() + " && tar xf tixs.tar")

hpcsample = 'hpc_sample'

test('hpc_help', normal, run_command, [hpc + ' help'])
test('hpc_help_help', normal, run_command, [hpc + ' help help'])

test('hpc_help_report', normal, run_command, [hpc + ' help report'])
test('hpc_report_001', normal, run_command, \
	[hpc + ' report ' + hpcsample + '.tix --xml-output']) 
test('hpc_report_002', normal, run_command, \
	[hpc + ' report ' + hpcsample + '.tix --xml-output --per-module']) 
test('hpc_report_003', normal, run_command, \
	[hpc + ' report ' + hpcsample + '.tix --per-module --decl-list']) 
test('hpc_report_error_001', exit_code(1), run_command, [hpc + ' report']) # no .tix file
test('hpc_report_error_002', exit_code(1), run_command, [hpc + ' report hpc001.hs']) # bad .tix file

test('hpc_help_markup', normal, run_command, [hpc + ' help markup'])
test('hpc_markup_error_001', exit_code(1), run_command, [hpc + ' markup']) # no .tix file
test('hpc_markup_error_002', exit_code(1), run_command, [hpc + ' markup hpc001.hs']) # bad .tix file
test('hpc_markup_001', normal, run_command, \
	['(' + hpc + ' markup ' + hpcsample + '.tix --highlight-covered ; cat Main.hs.html )']) 
test('hpc_markup_002', normal, run_command, \
	['(' + hpc + ' markup ' + hpcsample + '.tix --fun-entry-count ; cat Main.hs.html )']) 

test('hpc_help_show', normal, run_command, [hpc + ' help show'])
test('hpc_show', normal, run_command, [hpc + ' show ' + hpcsample + '.tix'])
test('hpc_show_error_001', exit_code(1), run_command, [hpc + ' show']) # no .tix file
test('hpc_show_error_002', exit_code(1), run_command, [hpc + ' show hpc001.hs']) # bad .tix file

test('hpc_help_version', normal, run_command, [hpc + ' help version'])
test('hpc_version', normal, run_command, [hpc + ' version'])

test('hpc_help_draft', normal, run_command, [hpc + ' help draft'])
test('hpc_draft', normal, run_command, [hpc + ' draft ' + hpcsample + '.tix'])

test('hpc_help_overlay', normal, run_command, [hpc + ' help overlay'])
test('hpc_overlay', normal, run_command, 
			[     hpc + ' overlay sample_overlay.ovr > sample_overlay.tix' +
			';  cat sample_overlay.tix ' +
			';' + hpc + ' combine ' + hpcsample + '.tix sample_overlay.tix > total.tix' + 
			';' + hpc + ' report total.tix' +
			'']);
test('hpc_overlay2', normal, run_command, 
			[     hpc + ' overlay sample_overlay.ovr --output=sample_overlay.tix' +
			';  cat sample_overlay.tix ' +
			';' + hpc + ' combine ' + hpcsample + '.tix sample_overlay.tix --output=total.tix' + 
			';' + hpc + ' report total.tix' +
			'']);
test('hpc_hand_overlay', normal, run_command, 
			[     hpc + ' overlay hand_overlay.ovr --output=sample_overlay.tix' +
			';  cat sample_overlay.tix ' +
			';' + hpc + ' combine ' + hpcsample + '.tix sample_overlay.tix --output=total.tix' + 
			';' + hpc + ' report total.tix' +
			'']);

test('hpc_bad_001', exit_code(1), run_command, [hpc + ' bad arguments'])
