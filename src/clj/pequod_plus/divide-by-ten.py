#!/usr/bin/python3

import re

pollutant_pattern = r':pollutants'
semaphore = False
new_content = ""

with open('./ppex004.clj', "r") as f_in:
    with open("ppex004-revised.clj", 'w') as f_out:
        for line in f_in:
            if semaphore == True:
                new_content = re.sub(":exponent 0.", ":exponent 0.0", line)
                semaphore = False
            else:
                new_content = line
            if re.search(pollutant_pattern, line) != None:
                semaphore = True
            f_out.write(new_content)
