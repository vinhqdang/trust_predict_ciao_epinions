lines = [line.rstrip('\n') for line in open('u.item')]

for line in lines:
    line = line.split ("|")
    x = line [-19:]
    with open("u.item2", "a") as myfile:
        myfile.write (" ".join (x) + "\n")
