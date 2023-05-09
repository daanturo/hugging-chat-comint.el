#!/usr/bin/env python

import argparse

import hugchat

parser = argparse.ArgumentParser()

parser.add_argument("--model")

args = parser.parse_args()

hugchat.cli(args=args, hello=True)
