pacman::p_load(here) # load here
set_here() # set your working dir, ie the .here
dr_here() # find where the .here is
here() # print the .here path 

# for the below example, I have a file located in the /lookhere folder called 'heretest.txt'
# "/Users/Documents/UsefulCode" # this is where the .here is set
readLines(here("heretest.txt")) # this fails because it can't find the txt file
readLines(here("lookhere","heretest.txt")) # this works because you specify the folder as the first argument (/lookhere)

# my summary: it can save you running into broken links, but it dones't seem any 
 # more efficient than just writing out the working dir as with setwd() 
  # because you need to know the subfolder names anyway. For example:

# "/Users/Documents/UsefulCode" # working dir
# this is the same ...
readLines(here("lookhere","heretest.txt")) # need to know the name of the /lookhere folder
# as this ...
readLines("/Users/Documents/UsefulCode/lookhere/heretest.txt")
# and this
readLines(paste0(getwd(),"/lookhere/heretest.txt"))

# unless I'm missing something amazing here   



