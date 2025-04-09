# Ebola Virus Sequence Evolution Simulation
# Based on a known transmission chain

# Packages ---------------------------------------------------------------
library(ape)
library(rentrez)
library(seqinr)
library(phangorn)

# Paths ------------------------------------------------------------------

output <- here::here("data", "ebola")

# Import simulated outbreak data -------------------------------------------------------

epi_sim <- readRDS(here::here("data", "ebola", "simulated_epicontact.rds"))

transmission_data <- epi_sim$contacts

# Seed -------------------------------------------------------------------

set.seed(123)

# Parameters -------------------------------------------------------------

# function to fecth a reference sequence
fetch_ref <- function(seq_id) {
  # Fetch the sequence
  ebola_fetch <- entrez_fetch(
    db = "nucleotide",
    id = seq_id,
    rettype = "fasta"
  )
  ebola_seq_text <- strsplit(ebola_fetch, "\n")[[1]]
  sequence_text <- paste(ebola_seq_text[-1], collapse = "") # Remove header line

  # Convert to character vector for seqinr
  ebola_ref_seq <- s2c(sequence_text)
  return(ebola_ref_seq)
}

# fecth reference sequence
ebola_ref_seq <- fetch_ref("MH733478")

# Set the evolutionary parameters for Ebola virus
# Approximate values based on literature
mutation_rate <- 9.86e-6 # substitutions per site per day
genome_length <- length(ebola_ref_seq)

# Function to convert time to expected mutations
get_expected_mutation <- function(time_days) {
  expected_mutations <- time_days * mutation_rate * genome_length
  return(expected_mutations)
}

# Simulate sequences -----------------------------------------------------

# Store all sequences in a list, with case IDs as names
sequences <- list()
sequences[["1"]] <- ebola_ref_seq

# Function to mutate a sequence based on expected mutations and a parent sequence
mutate_sequence <- function(parent_seq, expected_mutations) {
  # Make a copy of the parent sequence
  new_seq <- parent_seq

  if (expected_mutations > 0) {
    # Randomly select positions to mutate
    positions <- sample(
      1:length(parent_seq),
      expected_mutations,
      replace = FALSE
    )

    # For each position, replace with a random nucleotide (different from original)
    for (pos in positions) {
      current_base <- parent_seq[pos]
      possible_bases <- c("a", "c", "g", "t")
      possible_bases <- possible_bases[possible_bases != current_base]
      new_seq[pos] <- sample(possible_bases, 1)
    }
  }

  return(new_seq)
}

# Sort by infection time to ensure we process in chronological order
transmission_data <- transmission_data[order(transmission_data$t_inf), ]

# Simulate sequences for each case in the chain
for (i in 1:nrow(transmission_data)) {
  from_id <- as.character(transmission_data$from[i])
  to_id <- as.character(transmission_data$to[i])
  branch_length <- transmission_data$branch_length[i]

  # Get the parent sequence
  parent_seq <- sequences[[from_id]]

  # Calculate expected mutations based on branch length
  expected_mutations <- get_expected_mutation(branch_length)

  # Generate the new sequence with mutations
  new_seq <- mutate_sequence(parent_seq, expected_mutations)

  # Store the new sequence
  sequences[[to_id]] <- new_seq
}

# Convert to DNAbin format for tree construction

# Create a list of character vectors
dna_list <- lapply(sequences, function(seq) toupper(seq))

# Convert to DNAbin
dna_sequences <- as.DNAbin(dna_list)

# Calculate genetic distances
dist_matrix <- dist.dna(dna_sequences, model = "JC69")

# Construct and plot phylogenetic tree
# Build a neighbor-joining tree
nj_tree <- nj(dist_matrix)

plot(
  nj_tree,
  main = "Phylogenetic Tree of Simulated Ebola Sequences",
  cex.main = 1.5,
  cex = 0.8,
  edge.width = 2,
  tip.color = "blue"
)

write.dna(
  dna_sequences,
  file = here::here(output, "simulated_sequences.fasta"),
  format = "fasta",
  colsep = ""
)

saveRDS(dna_sequences, here::here(output, "simulated_sequences.rds"))
