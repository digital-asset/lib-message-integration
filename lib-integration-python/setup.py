import setuptools

setuptools.setup(
    name="message_integration",
    version="0.0.1",
    author="Thomas Weber",
    author_email="thomas.weber@digitalasset.com",
    description="Message integration library",
    url="https://github.com/digital-asset/lib-message-integration",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
