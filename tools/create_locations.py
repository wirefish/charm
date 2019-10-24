#!/usr/bin/env python3

import sys

input_path = sys.argv[1]
output_path = sys.argv[2]

# The input file consists of a set of lines that define location and portal
# prototypes, followed by a blank line, followed by a grid representation of the
# map.
#
# Each prototype line consists of a map character, a location prototype, and an
# optional portal prototype.

prototypes = {}
zone_name = None

with open(input_path) as f:

    zone_name = f.readline().strip()

    for line in f:
        if line == "\n":
            break
        map_char, location_proto, portal_proto = line.split()
        prototypes[map_char] = location_proto, portal_proto

    grid = f.readlines()

def grid_char(i, j):
    if j < 0 or j >= len(grid):
        return " "
    row = grid[j]
    if i < 0 or i >= len(row):
        return " "
    else:
        return row[i]

def grid_location(i, j):
    return "{}-{}-{}".format(
        prototypes[grid_char(i, j)][0],
        i // 2,
        j // 2)

def find_exits(i, j):
    exits = []
    if grid_char(i, j - 1) == "|":
        exits.append(["north", 0, -1])
    if grid_char(i, j + 1) == "|":
        exits.append(["south", 0, 1])
    if grid_char(i - 1, j) == "-":
        exits.append(["west", -1, 0])
    if grid_char(i + 1, j) == "-":
        exits.append(["east", 1, 0])
    if grid_char(i - 1, j - 1) in ["\\", "X"]:
        exits.append(["northwest", -1, -1])
    if grid_char(i + 1, j - 1) in ["/", "X"]:
        exits.append(["northeast", 1, -1])
    if grid_char(i - 1, j + 1) in ["/", "X"]:
        exits.append(["southwest", -1, 1])
    if grid_char(i + 1, j + 1) in ["\\", "X"]:
        exits.append(["southeast", 1, 1])
    return exits

PROTO_FORMAT = "(defproto {} ({}))\n\n"

LOCATION_FORMAT = "(deflocation {} ({})\n  (exits (({} {}))))\n\n"

with open(output_path, "w") as f:

    f.write("(in-package :{})\n\n(defregion {})\n\n".format(
        zone_name, zone_name))

    for location_proto, portal_proto in prototypes.values():
        f.write(PROTO_FORMAT.format(location_proto, "location"))
        f.write(PROTO_FORMAT.format(portal_proto, "portal"))

    num_locations = 0
    for j in range(0, len(grid), 2):
        row = grid[j]
        for i in range(0, len(row), 2):
            map_char = row[i]
            if map_char != " ":
                location_proto, portal_proto = prototypes[map_char]
                exits = find_exits(i, j)
                f.write(LOCATION_FORMAT.format(
                    grid_location(i, j), location_proto, portal_proto,
                    " ".join(
                        ":{} {}".format(direction, grid_location(i + 2 * dx, j + 2 * dy))
                        for direction, dx, dy in exits)))
                num_locations += 1

    print("defined {} locations".format(num_locations))
