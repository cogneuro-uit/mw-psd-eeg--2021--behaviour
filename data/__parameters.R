nback=25
which.apen=2

probe_information <- list(
  questions = list(
    probe1 = "I hvilken grad fokuserte du på oppgaven rett før dette spørsmålet?",
    probe2 = "I den grad du ikke fokuserte på oppgaven, tenkte du på ingenting eller tenkte du på noe spesifikt?",
    probe3 = "Var du bevisst på hvor du fokusserte oppmerksomheten din (enten oppgaverelatert eller annet) eller var den spontan?"
  ),
  descriptions = list(
    probe1 = "1 = helt klart IKKE fokussert; 4 = helt klart fokussert",
    probe2 = "1 = helt klart ingenting; 4 = helt klart noe spesifikt",
    probe3 = "1 = helt klart spontan; 4 = helt klart bevisst"
  )
)


theme_set(
  theme_bw() 
)

options( 
  mc.cores = floor(parallel::detectCores()*.8) 
  
)
