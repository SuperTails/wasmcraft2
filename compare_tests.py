import subprocess
from typing import List

def list_integ_tests() -> List[str]:
	lister = subprocess.run(['cargo', 'test', '--', '--list'], capture_output=True, text=True)

	assert lister.returncode == 0

	idx = lister.stdout.index('\n\n')
	after = lister.stdout[idx:]
	idx = after[:after.index(': test')].rindex('\n')

	integ_tests = after[idx:]
	integ_tests = integ_tests[:integ_tests.index('\n\n')]

	test_names = integ_tests.split('\n')

	assert test_names[0] == ''

	test_names = [t.removesuffix(': test') for t in test_names[1:]]

	return test_names

TEST_PASSED = 0
TEST_FAILED = 1
TEST_IGNORED = 2
TEST_UNKNOWN = 3

def run_normal_test(name: str) -> int:
	tester = subprocess.run(['cargo', 'test', name, '--', '--exact'], capture_output=True, text=True)

	passed = tester.stdout.count('1 passed')
	failed = tester.stdout.count('1 failed')
	ignored = tester.stdout.count('1 ignored')
	assert passed in (0, 1)
	assert failed in (0, 1)
	assert ignored in (0, 1)

	passed = passed == 1
	failed = failed == 1
	ignored = ignored == 1

	assert passed + failed + ignored == 1

	if passed:
		return TEST_PASSED
	elif failed:
		return TEST_FAILED
	elif ignored:
		return TEST_IGNORED
	else:
		raise Exception("what")

def run_server_test(name: str) -> int:
	tester = subprocess.run(['cargo', 'test', name, '--features=servertests', '--', '--exact', '--test-threads=1'], capture_output=True, text=True)

	passed = tester.stdout.count('1 passed')
	failed = tester.stdout.count('1 failed')
	ignored = tester.stdout.count('1 ignored')
	assert passed in (0, 1)
	assert failed in (0, 1)
	assert ignored in (0, 1)

	passed = passed == 1
	failed = failed == 1
	ignored = ignored == 1

	assert passed + failed + ignored == 1

	if passed:
		return TEST_PASSED
	elif failed:
		return TEST_FAILED
	elif ignored:
		return TEST_IGNORED
	else:
		raise Exception("what")


def main():
	tests = list_integ_tests()

	passed = set()
	failed = set()
	ignored = set()

	for test in tests:
		result = run_normal_test(test)
		if result == TEST_PASSED:
			passed.add(test)
		elif result == TEST_FAILED:
			failed.add(test)
		elif result == TEST_IGNORED:
			ignored.add(test)
	
	print(passed)
	print(len(passed))

	for test in passed:
		result = run_server_test(test)
		if result != TEST_PASSED:
			print(f'TEST {result} DIFFERED!!')
	
	print('Done')

if __name__ == '__main__':
	main()
