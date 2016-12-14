import csv
import sys

def simple(input_file, output_file):
    result = {}
    for line in csv.reader(open(input_file)):
        if line[0] in result:
            result[line[0]] += int(line[2])
        else:
            result[line[0]] = int(line[2])

    with open(output_file, 'w') as out:
        writer = csv.writer(out)
        for res in result.items():
            writer.writerow(res)


if __name__ == '__main__':
    simple(sys.argv[1], sys.argv[2])
