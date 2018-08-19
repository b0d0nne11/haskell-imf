from setuptools import setup, find_packages

setup(
    name="imf",
    version="0.3.0",
    description="Utilities for parsing and manipulating email messages",
    url="https://github.com/b0d0nne11/imf",
    package_dir={"": "python"},
    packages=find_packages("python", exclude=["tests"]),
    tests_require=["coverage", "nose", "mock"],
)
