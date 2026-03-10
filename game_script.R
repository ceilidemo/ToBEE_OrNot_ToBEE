###############################################################
# TO BEE OR NOT TO BEE
# Ceili DeMarais lay summary for ENTO 510
# Based on Wang & Tan (2019) Insects 10(10): 336
###############################################################


# ---- Game functions ----------------------------------------

press_enter <- function() {
  invisible(readline(prompt = "\n  [ Press ENTER (return on mac) to continue... ] "))
}

divider <- function() {
  cat("\n", paste(rep("~", 60), collapse = ""), "\n\n")
}

narrate <- function(text) {
  cat("\n", text, "\n")
}

show_backpack <- function(backpack) {
  cat("\n  Thou backpack contains:\n")
  if (length(backpack) == 0) {
    cat("  (empty)\n")
  } else {
    for (i in seq_along(backpack)) {
      cat(paste0("  [", i, "] ", backpack[[i]]$name, "\n"))
      cat(paste0("       Hint: ", backpack[[i]]$hint, "\n"))
    }
  }
}

pick_option <- function(prompt_text, n_options) {
  repeat {
    cat(prompt_text)
    choice <- suppressWarnings(as.integer(readline(prompt = "  > ")))
    if (!is.na(choice) && choice >= 1 && choice <= n_options) {
      return(choice)
    }
    cat("  Enter a number between 1 and", n_options, "\n")
  }
}

die <- function(funny_msg, science_msg) {
  divider()
  cat("\n  *** YOU DIED ***\n\n")
  cat(" ", funny_msg, "\n")
  cat("\n  --- Why you died... ---\n")
  cat(" ", science_msg, "\n")
  press_enter()
  return(FALSE)
}

win_phase <- function(msg, science_msg) {
  divider()
  cat("\n  *** Survived to live another hour ***\n\n")
  cat(" ", msg, "\n")
  cat("\n  --- Why: ---\n")
  cat(" ", science_msg, "\n")
  divider()
  press_enter()
  return(TRUE)
}

# Picking pheromone from backpack
pick_from_backpack <- function(backpack, include_nothing = TRUE) {
  cat("\n  What do you release from your backpack?\n")
  bp_keys <- names(backpack)
  for (i in seq_along(bp_keys)) {
    cat(paste0("[", i, "] ", backpack[[bp_keys[i]]]$name, "\n"))
    cat(paste0("Hint: ", backpack[[bp_keys[i]]]$hint, "\n"))
  }
  if (include_nothing) { # give option to not release anything
    cat(paste0("[", length(bp_keys) + 1, "] Actually... release nothing\n"))
    choice <- pick_option("  > ", length(bp_keys) + 1)
    if (choice == length(bp_keys) + 1) return("nothing")
  } else {
    choice <- pick_option("  > ", length(bp_keys))
  }
  return(bp_keys[choice]) #returns you to the other choices
}


# ---- All Pheromones ----------------------------------------

all_pheromones <- list(
  
  isopentyl_acetate = list(
    name   = "* BEST SELLER * 'Isopentyl Acetate' (Call for Backup!)",
    smell  = "Ripe bananas. Every bee knows this one.",
    hint   = "RECRUITS nestmates & triggers stinging. Works in ALL Apis species. WARNING: also attracts hive beetles.",
    effect = "recruit_and_sting"
  ),
  
  benzyl_acetate = list(
    name   = "'Benzyl Acetate' (Hive Alert & Forager Repellent)",
    smell  = "Sweet, floral, and little like jasmine.",
    hint   = "ALERTS bees at hive entrance. Mainly repells and yells *DANGER*. Main alarm compound of A. cerana.",
    effect = "alert_and_repel"
  ),
  
  two_heptanone = list(
    name   = "'2-Heptanone' (Flower already TAKEN!)",
    smell  = "Faintly cheesy, like sharp cheddar.",
    hint   = "MARKS flowers as already foraged. NOT a nestmate recruiter.",
    effect = "forage_mark"
  ),
  
  gamma_octanoic_lactone = list(
    name   = "'Gamma-Octanoic Lactone' (Repel the big bee)",
    smell  = "Oily, like a coconut.",
    hint   = "REPELS foragers from food sources. Giant bee pheromone...but YOU can eavesdrop on it too and somehow BEEtrice sells it...",
    effect = "giant_bee_repel"
  ),
  
  z11_eicosen_1_ol = list(
    name   = "'(Z)-11-eicosen-1-ol' (Boosts your other signals)",
    smell  = "Waxy smell",
    hint   = "TRIGGERS stinging AND carries/enhances other alarm compounds. Best paired with others.",
    effect = "sting_trigger"
  ),
  
  hexyl_acetate = list(
    name   = "'Hexyl Acetate' (Recruit without stinging!)",
    smell  = "Fresh and fruity",
    hint   = "RECRUITS nestmates to a threat but does not itself elicit stinging. Gentler than isopentyl_acetate.",
    effect = "recruit_only"
  )
)


# ---- Pheromone Store (Perfumary?) --------------------------------------------

visit_perfumary <- function() {
  
  divider()
  cat("
  WELCOME TO THE PERFUMARY!
  
  Before you begin your tasks for the day, 
  you should stock up on some useful pheromones!

  The bell above the door jingles as you enter.
  Old Beetrice, the owner squints at you over her tiny spectacles.
  Each day she watches as bees leave the hive with pheromones from her shop...
  not all return...

  'Good morning young one. Three pheromones. Choose wisely.'

  She points her antennae towards the wall behind her.
  The shelves are overflowing with pheromones of all kinds. 
  

  'A recommentation to you: Diversify.
  Who knows what you may encounter today...
  best to be prepared for every situation.' 
  And...' she lowers her voice, '...some smells may attract
  those you were not hoping to... use wisely'

  She slides one item forward marked with 'BEST SELLER!'
  'This one I recommend... BUT I also recommend reading
   the fine print. Pheromones are not simple tools.'

  She pauses.
      ")
  
  press_enter()
  
  cat("\n  Available pheromones:\n\n")
  pheromone_keys <- names(all_pheromones)
  
  for (i in seq_along(pheromone_keys)) {
    p <- all_pheromones[[pheromone_keys[i]]]
    cat(paste0("  [", i, "] ", p$name, "\n"))
    cat(paste0("       Smell: ", p$smell, "\n"))
    cat(paste0("       Hint:  ", p$hint, "\n\n"))
  }
  
  backpack <- list()
  
  while (length(backpack) < 3) {
    slot <- length(backpack) + 1
    cat(paste0("\n  Choose pheromone number ", slot, " for your backpack: (type number)\n"))
    choice <- pick_option("", length(pheromone_keys))
    selected_key <- pheromone_keys[choice]
    
    if (selected_key %in% names(backpack)) {
      cat("  You already have a bottle and will not run out! Pick a different pheromone.\n")
    } else {
      backpack[[selected_key]] <- all_pheromones[[selected_key]]
      cat(paste0("  Added: ", backpack[[selected_key]]$name, "\n"))
    }
  }
  
  divider()
  cat("  Your backpack is packed. Here is what you have chosen:\n")
  show_backpack(backpack)
  press_enter()
  
  return(backpack)
}


# ---- PHASE 1: The Flower Patch -----------------------------
# foraging economics, recruitment vs marking
# dead body avoidance, kairomone setup for Phase 2

phase_flower_patch <- function(backpack) {
  
  # state variables that will come in later
  # I am thinking imma have these build up over the course of the day
  recruited_loudly  <- FALSE
  marked_flower     <- FALSE
  saw_dead_bee      <- FALSE
  beetle_risk       <- FALSE   # turns TRUE if isopentyl used here
  
  repeat {
    
    divider()
    cat("
  Early Morning: The Flower Patch 

  The sun is up. Time to Forage.
  You leave the hive with your backpack 
  and a determination to bring back nourishment for the hive. 

  A half mile away you find a Calliandra haematocephala flower in full bloom!
  From a quick glance you can tell this flower is prime stuff.

  BUT... before you land, you notice something familiar on the petals.
    ")
    
    cat("  [1] Look more closely before landing\n")
    cat("  [2] Land immediately! Nectar waits for no bee!\n")
    cat("  [3] Check your backpack first...\n")
    
    choice <- pick_option("\n  Your choice: ", 3)
    
    # backpack quick check
    if (choice == 3) {
      show_backpack(backpack)
      cat("\n  Now what?\n")
      cat("  [1] Look more closely\n")
      cat("  [2] Land immediately\n")
      choice <- pick_option("  > ", 2)
    }
    
    # Check out the situation choice
    if (choice == 1) {
      saw_dead_bee <- TRUE
      narrate("
  You hover closer and see... a dead bee?
  It smells faintly of a certain best elling alarm pheromone...
  Stale... fading... but unmistakable. 
  Something bad happened here.

  You glance around at the other flowers and see other bees flitting about. 
  Happily collecting the sweet sweet nectar. 
  You are tempted... 
  but  your reaction to the alarm pheromone causes you to think twice.")
      
      cat("  [1] Avoid this patch entirely. Find a quieter flower... with no dead bee\n")
      cat("  [2] Land anyway. The alarm smell is old and faint... probably fine\n")
      
      look_choice <- pick_option("\n  Your choice: ", 2)
      
      # Chose to avoid the patch
      if (look_choice == 1) {
        narrate("
  You find a smaller, less spectacular Lantana nearby.
  Not as much nectar. But quiet. No dead bees. No old alarm smell.
  You forage alone and unbothered.")
        
        cat("\n  What will you do with this forage site?\n")
        cat("  [1] Release a recruitment signal: tell nestmates about it\n")
        cat("  [2] Mark it with a forage marker: tell others you visited and collected what is here\n")
        cat("  [3] Forage quietly with no signal at all\n")
        
        forage_choice <- pick_option("  > ", 3)
        
        if (forage_choice == 1) {
          # recruit : nestmates come but less reward
          pheromone_used <- pick_from_backpack(backpack)
          if (pheromone_used %in% c("isopentyl_acetate", "hexyl_acetate", "z11_eicosen_1_ol")) {
            recruited_loudly <- TRUE
            if (pheromone_used == "isopentyl_acetate") beetle_risk <- TRUE
            narrate("
  Nestmates arrive. 
  Less nectar than the other patch, but safer... maybe?
  You notice some weaver ants in the area taking interest
  in the increased bee activity. They set up nearby. Watching.")
            win_phase(
              "You foraged safely on a quieter patch. Colony gets less food but you are alive!",
              "Honey bees balance foraging costs and benefits continuously.
  Dead bee bodies near flowers are genuine risk cues. 
  Bees may avoid flowers with alarm pheromone or dead nestmates present,
  even if the nectar is oh so sweet.
  (Wang & Tan 2019, Section 3.1)"
            )
            return(list(
              backpack         = backpack,
              recruited_loudly = recruited_loudly,
              marked_flower    = FALSE,
              saw_dead_bee     = saw_dead_bee,
              beetle_risk      = beetle_risk,
              quiet_patch      = TRUE
            ))
          } else if (pheromone_used == "two_heptanone") {
            narrate("
  You release the forage marker. It tells nestmates: visited, move along.
  They move along to find other sites. You forage alone. Efficient... but lonely.")
            marked_flower <- TRUE
            win_phase(
              "You marked the flower instead of recruiting. Correct use of 2-heptanone.",
              "2-Heptanone from the mandibular gland functions as a forage-marking
  pheromone. It tells other bees a flower has already been visited 
  and gives them an opportunity to go find other (possibly better) forage sources.
  (Wang & Tan 2019, Section 2.1)"
            )
            return(list(
              backpack         = backpack,
              recruited_loudly = FALSE,
              marked_flower    = TRUE,
              saw_dead_bee     = saw_dead_bee,
              beetle_risk      = FALSE,
              quiet_patch      = TRUE
            ))
          } else {
            # Wrong pheromone...
            narrate("
  You release your pheromone. It does not recruit nestmates effectively.
  The signal drifts off. Nothing comes. You forage alone anyway.")
            win_phase(
              "You survived but the pheromone you chose was not suited for recruitment.",
              "Not all alarm pheromone compounds recruit nestmates. Isopentyl acetate
  and hexyl acetate are the primary recruiters. Other compounds serve
  different functions (Check out the hint/ fine print on the bottles!). 
  (Wang & Tan 2019, Table 1)")
            return(list(
              backpack         = backpack,
              recruited_loudly = FALSE,
              marked_flower    = FALSE,
              saw_dead_bee     = saw_dead_bee,
              beetle_risk      = FALSE,
              quiet_patch      = TRUE
            ))
          }
          
        } else if (forage_choice == 2) {
          # Mark it: say I was here
          if ("two_heptanone" %in% names(backpack)) {
            marked_flower <- TRUE
            narrate("
  You release 2-heptanone: the forage marker. It says to other bees:
  I was here. Flower visited. Move along.
  Your hivemates will know to look elsewhere.
  You collect what nectar there is and prepare to move along.")
          } else {
            narrate("
  You do not have a forage marker in your backpack.
  You forage alone without signalling anything to anyone.")
          }
          win_phase(
            "Quiet foraging complete. Low yield but no major issues.",
            "2-Heptanone marks visited flowers, improving colony foraging efficiency
  by preventing multiple bees from visiting the same depleted source.
  (Wang & Tan 2019, Section 2.1)"
          )
          return(list(
            backpack         = backpack,
            recruited_loudly = FALSE,
            marked_flower    = marked_flower,
            saw_dead_bee     = saw_dead_bee,
            beetle_risk      = FALSE,
            quiet_patch      = TRUE
          ))
          
        } else {
          # Do NADA at quiet flower
          narrate("
  You forage quietly. No signals. No communication.
  Just you, the Lantana, and your thoughts.")
          win_phase(
            "Silent foraging. Safe but minimal contribution to the colony.",
            "Honey bees use pheromone signals to coordinate foraging efficiently.
  Without any signal, foraging is uncoordinated and less productive.
  (Wang & Tan 2019, Introduction)"
          )
          return(list(
            backpack         = backpack,
            recruited_loudly = FALSE,
            marked_flower    = FALSE,
            saw_dead_bee     = saw_dead_bee,
            beetle_risk      = FALSE,
            quiet_patch      = TRUE
          ))
        }
        
      } else {
        # Saw dead bee. bold choise and landed anyways
        narrate("
  You land anyway. The nectar really is oh so sweet!
  The dead bee smell is old. You're probably fine... right?")
      }
    }
    
    ########################
    # Landed on main flower 
    cat("
  You landed. The nectar is quite wonderful.
  What will you do with this forage site?
    ")
    cat("  [1] Release a pheromone: recruit nestmates to this patch\n")
    cat("  [2] Release a pheromone: mark it as visited\n")
    cat("  [3] Forage quietly with no signal at all\n")
    cat("  [4] Check your backpack\n")
    
    forage_choice <- pick_option("\n  Your choice: ", 4) #check backpack
    
    if (forage_choice == 4) {
      show_backpack(backpack)
      cat("\n  Now what?\n")
      cat("  [1] Recruit nestmates\n")
      cat("  [2] Mark as visited\n")
      cat("  [3] Forage quietly\n")
      forage_choice <- pick_option("  > ", 3)
    }
    
    if (forage_choice == 1) {
      # Recruit
      pheromone_used <- pick_from_backpack(backpack)
      
      if (pheromone_used == "nothing") {
        narrate("  You decide against releasing anything. You forage alone.")
        win_phase(
          "Quiet foraging. Safe for now.",
          "Without recruitment signals, foraging is uncoordinated and lacks efficiency.
  (Wang & Tan 2019, Introduction)"
        )
        return(list(
          backpack         = backpack,
          recruited_loudly = FALSE,
          marked_flower    = FALSE,
          saw_dead_bee     = saw_dead_bee,
          beetle_risk      = FALSE,
          quiet_patch      = FALSE
        ))
      }
      
      # RECRUIT pheromone
      if (pheromone_used %in% c("isopentyl_acetate", "hexyl_acetate",
                                "z11_eicosen_1_ol", "benzyl_acetate")) {
        recruited_loudly <- TRUE
        if (pheromone_used == "isopentyl_acetate") beetle_risk <- TRUE
        narrate("
  You release your pheromone. Within minutes, hivemates arrive.
  The prime stop is swept clean! SO much good forage collected.

  BUT you notice movement near the flowers....
  One or two weaver ants. Watching the increased bee activity.
  They are patient. They are waiting.")
        win_phase(
          "Efficient foraging! Colony is well fed. But, something noticed your signal.",
          "Recruitment pheromones like isopentyl acetate increase foraging efficiency
  but also increase activity that predators can detect. Weaver ants use
  bee alarm pheromone as a kairomone. They may eavesdrop on bee signals to
  locate prey. Increased bee traffic can also increase this predator interest.
  (Wang & Tan 2019, Section 3.1)"
        )
        return(list(
          backpack         = backpack,
          recruited_loudly = TRUE,
          marked_flower    = FALSE,
          saw_dead_bee     = saw_dead_bee,
          beetle_risk      = beetle_risk,
          quiet_patch      = FALSE
        ))
        
        # marked it as foraged.. move along
      } else if (pheromone_used == "two_heptanone") {
        narrate("
  You release 2-heptanone at the best flower patch this side of the Mississippi!
  It smells faintly cheesy. Your nestmates, smellin it,
  interpret it correctly: visited, move along.
  They move along. To other, possibly worse, flowers.
  You forage alone on this spectacular patch. How rude.")
        marked_flower <- TRUE
        win_phase(
          "You marked the BEST flower as already visited. Oops!
          There is no way you can forage ALL this nectar alone... guess the hive will miss out",
          "2-Heptanone functions as a forage-marking pheromone telling nestmates
  to move on from a visited flower, not a recruitment signal. Using it
  at a rich patch is a costly mistake for colony foraging efficiency.
  (Wang & Tan 2019, Section 2.1)"
        )
        return(list(
          backpack         = backpack,
          recruited_loudly = FALSE,
          marked_flower    = TRUE,
          saw_dead_bee     = saw_dead_bee,
          beetle_risk      = FALSE,
          quiet_patch      = FALSE
        ))
        
        #super strong one cause could be fun
      } else if (pheromone_used == "gamma_octanoic_lactone") {
        narrate("
  You release gamma-octanoic lactone at your flower patch.
  Every forager within range veers sharply away.
  Including the ones who were already on their way here.
  You have successfully repelled your entire workforce
  from the best nectar source of the season.")
        win_phase(
          "You repelled your own nestmates from a great food source. The colony is hungry.",
          "Gamma-octanoic lactone is highly repellent to foragers at food sources.
  It evolved in giant honey bee species to keep competitors away.
  Using it at your own patch is spectacularly counterproductive.
  (Wang & Tan 2019, Section 2.2)"
        )
        return(list(
          backpack         = backpack,
          recruited_loudly = FALSE,
          marked_flower    = FALSE,
          saw_dead_bee     = saw_dead_bee,
          beetle_risk      = FALSE,
          quiet_patch      = FALSE
        ))
      }
    }
    
    if (forage_choice == 2) {
      pheromone_used <- pick_from_backpack(backpack)
      
      if (pheromone_used == "nothing") { # do nada
        narrate("  You decide not to mark it. You forage alone in silence.")
        win_phase(
          "No smell, no consequences. Yet...",
          "Silent foraging avoids attracting predators but also misses
  opportunities for colony coordination! (Wang & Tan 2019, Introduction)"
        )
        return(list(
          backpack         = backpack,
          recruited_loudly = FALSE,
          marked_flower    = FALSE,
          saw_dead_bee     = saw_dead_bee,
          beetle_risk      = FALSE,
          quiet_patch      = FALSE
        ))
      }
      
      if (pheromone_used == "two_heptanone") {
        marked_flower <- TRUE
        narrate("
  You release 2-heptanone at the best flower patch this side of the Mississippi!
  It smells faintly cheesy. Your nestmates, smellin it,
  interpret it correctly: visited, move along.
  They move along. To other, possibly worse, flowers.
  You forage alone on this spectacular patch. How rude.

  In the distance, a very large bee (A. dorsata, a giant honey bee)
  is working flowers nearby. You can detect something faintly
  chemical about it. An unfamiliar compound drifting your way.")
        
        win_phase(
          "Flower marked correctly. You forage alone and undisturbed. For now.",
          "2-Heptanone as a forage marker keeps the patch uncrowded and
  reduces the chemical signals that could attract predators. 
  BUT it also limits the amount of good forage your hive could collect 
  and limits efficiency! You just cost your hive some GREAT nectar.
  
  A. cerana can detect alarm compounds from other Apis species,
  including giant bees, showing inter-species eavesdropping.
  (Wang & Tan 2019, Sections 2.1 and 2.2)"
        )
        return(list(
          backpack         = backpack,
          recruited_loudly = FALSE,
          marked_flower    = TRUE,
          saw_dead_bee     = saw_dead_bee,
          beetle_risk      = FALSE,
          quiet_patch      = FALSE
        ))
        
      } else {
        # Wrong compound for marking
        narrate("
  You release your pheromone. It is NOT a forage marker.
  Your nestmates receive a confusing signal and respond
  in various unhelpful ways. Some come. Some flee.
  You forage amid the chaos.")
        win_phase(
          "Wrong compound for marking, but you survived the confusion.",
          "Different pheromone compounds serve specific functions. Using an
  alarm or recruitment compound when you mean to mark a flower
  sends the wrong message entirely. (Wang & Tan 2019, Table 1)"
        )
        return(list(
          backpack         = backpack,
          recruited_loudly = pheromone_used %in% c("isopentyl_acetate",
                                                   "hexyl_acetate",
                                                   "z11_eicosen_1_ol"),
          marked_flower    = FALSE,
          saw_dead_bee     = saw_dead_bee,
          beetle_risk      = pheromone_used == "isopentyl_acetate",
          quiet_patch      = FALSE
        ))
      }
    }
    
    if (forage_choice == 3) {
      # Silent foraging
      narrate("
  You forage quietly. No signals. No communication.
  The nectar is excellent. You fill your baskets in peace.
  No ants come. No nestmates come either.
  The colony gets a modest solo contribution.")
      win_phase(
        "Silent foraging complete. Safe but uncoordinated.",
        "Foraging without signals avoids attracting predators but also
  limits colony efficiency. Honey bees balance these costs and benefits
  constantly during foraging trips. (Wang & Tan 2019, Introduction)"
      )
      return(list(
        backpack         = backpack,
        recruited_loudly = FALSE,
        marked_flower    = FALSE,
        saw_dead_bee     = saw_dead_bee,
        beetle_risk      = FALSE,
        quiet_patch      = FALSE
      ))
    }
    
  } # END thee repeat
}


# ---- PHASE 2: Eek ... ants... or spider --------------------
# ant kairomone eavesdropping, inter-species detection
# cost of stinging, sit-and-wait predators
# the predator depends on choices you made above

phase_something_arrives <- function(backpack, phase1) {
  
  recruited_loudly <- phase1$recruited_loudly
  marked_flower    <- phase1$marked_flower
  saw_dead_bee     <- phase1$saw_dead_bee
  quiet_patch      <- phase1$quiet_patch
  
  # Here you get giant bee, spide, or ants based on phase 1 choices
  if (quiet_patch) {
    encounter <- "spider"
  } else if (recruited_loudly) {
    encounter <- "ants"
  } else if (marked_flower) {
    encounter <- "giant_bee"
  } else {
    encounter <- "spider"  
  }
  
  repeat {
    
    divider()
    
    # ANTS
    if (encounter == "ants") {
      cat("
  Midmorning: The ambush...

  You are still foraging. The flower is spectacular.

  Then... you feel it. OW! Tiny mandibles on your rear leg.
  A weaver ant. Just one. But you can see others waiting
  in the twigs 20-30 cm away. They set up while you were busy... distracted!

  They were attracted by the activity your recruitment signal created.
  They were waiting for exactly this moment.

  Now you have a choice... 
  If you release alarm pheromone, YOUR nestmates will come.
  But ants use bee alarm pheromone as a KAIROMONE.
  They will also come. Faster.

  What do you do?
      ")
      
      cat("  [1] Release a pheromone from your backpack\n")
      cat("  [2] Perform the waggle dance: signal to the ants you see them\n")
      cat("  [3] Stay completely still... release nothing\n")
      cat("  [4] Sting the ant. Go out fighting!\n")
      cat("  [5] Check your backpack\n")
      
      choice <- pick_option("\n  Your choice: ", 5)
      
      if (choice == 5) {
        show_backpack(backpack)
        cat("\n  Now what?\n")
        cat("  [1] Release a pheromone\n")
        cat("  [2] Waggle dance\n")
        cat("  [3] Stay still\n")
        cat("  [4] Sting the ant\n")
        choice <- pick_option("  > ", 4)
      }
      
      if (choice == 1) {
        pheromone_used <- pick_from_backpack(backpack)
        
        if (pheromone_used == "nothing") {
          choice <- 3  # fall through to stay still
        } else if (pheromone_used == "isopentyl_acetate") {
          narrate("
  The banana scent explodes into the air! POOF!
  Nestmates come running... they were already nearby from your recruitment!
  But the ants come FASTER. They also read that signal.
  Three more ants arrive before your first nestmate does.

  Your nestmates fight them off. Barely. Two nestmates are lost.
  You escape, shaken, with a bruised leg and moral complications.")
          return(win_phase(
            "You survived, but your alarm signal recruited the ants too.
  Two nestmates are dead. The ants eavesdropped on your distress.",
            "Ants use bee alarm pheromone as a kairomone. They eavesdrop on
  the bee's distress signal to locate and mob prey. Isopentyl acetate
  recruits nestmates but also alerts nearby predators simultaneously.
  (Wang & Tan 2019, Section 3.1)"
          ))
          
        } else if (pheromone_used %in% c("hexyl_acetate", "z11_eicosen_1_ol")) {
          narrate("
  You release your recruiter. Nestmates arrive quickly!
  they were already nearby from your earlier signal.
  The ant hesitates at the new chemical signal.
  The guards form a cluster. The ant retreats.
  One nestmate lost in the scuffle.")
          return(win_phase(
            "Survived with fewer casualties than the alarm would have caused.
  Hexyl acetate recruits without the full alarm blast that attracts more ants.",
            "Different recruitment compounds have different 'blast radii'.
  Hexyl acetate recruits nestmates without the full predator-attracting
  alarm signal of isopentyl acetate. 
  (Wang & Tan 2019, Table 1)"
          ))
          
        } else if (pheromone_used == "benzyl_acetate") {
          narrate("
  You release Jasmine Alarm. The hive entrance stirs,
  returning bees are alerted, but you are far from the hive....
  The signal takes time to translate into defenders at your location.
  Two more ants arrive before any help does.")
          return(die(
            "The signal alerted the hive but not fast enough for you personally.
  The ants had you surrounded before defenders arrived.",
            "Benzyl acetate primarily alerts bees at the hive entrance and
  repels foragers. It is less effective as a long-range distress signal
  than isopentyl acetate. Context and distance matter!
  (Wang & Tan 2019, Section 2.1)"
          ))
          
        } else if (pheromone_used == "two_heptanone") {
          narrate("
  You release 2-heptanone. It smells cheesy.
  The ant pauses, confused. A few nearby bees interpret the signal
  correctly and move away from the flower.
  Your help leaves. The ant's friends arrive.")
          return(die(
            "You forage-marked the situation. The flower is thoroughly tagged.
  Also you are dead.",
            "2-Heptanone is a forage marker, not a distress signal. It tells
  nestmates to move away from a location... sxactly the wrong message
  when you need them to come and help! (Wang & Tan 2019, Section 2.1)"
          ))
          
        } else if (pheromone_used == "gamma_octanoic_lactone") {
          narrate("
  You release gamma-octanoic lactone. Every bee in the vicinity
  flees from the food source. Including the ones who might have
  helped you. You are now alone with an ant.")
          return(die(
            "You cleared the field of allies. The ant appreciated the privacy.",
            "Gamma-octanoic lactone repels foragers from food sources.
  Releasing it when you need help drives your nestmates away.
  Wrong compound, catastrophic timing... (Wang & Tan 2019, Section 2.2)"
          ))
        }
      }
      
      if (choice == 2) {
        return(die(
          "You waggle-danced at an ant. It was a beautiful performance.
  The ant gave it 9/10. Then the friends waiting to ambush came along and they killed you.",
          "The waggle dance communicates food source locations and some danger
  signals between bees and also works as an 'I-see-you' signal to SOME predators, not all.
  It is not understood by ants and does not trigger rapid defensive recruitment 
  the way alarm pheromone does.Chemical signals are faster and more specific. 
          (Wang & Tan 2019, Introduction)"
        ))
      }
      
      if (choice == 3) {
        narrate("
  You freeze. Impressive self-control.
  The ant, still gripping your leg, releases its own trail pheromone.
  More ants come. They did not need your signal. They have their own.")
        return(die(
          "Stoic to the end. The ants had their own communication system
  and did not need your cooperation.",
          "Staying silent prevents bee alarm pheromone from alerting more ants,
  but ants use their own trail pheromones independently to coordinate attacks.
  Silence is not a complete solution when predators communicate too.
  (Wang & Tan 2019, Section 3.1)"
        ))
      }
      
      if (choice == 4) {
        return(die(
          "You stung the ant. Noble. Way to go out with a fight.
  Your barbed stinger is now lodged in the ant and you are
  currently eviscerating yourself. You die defending a flower.
  The ant also dies. A draw.",
          "Apis cerana has a barbed stinger. Stinging an insect target means
  the stinger becomes lodged and the bee dies in the process.
  This sacrificial defence makes sense at the hive entrance where it
  triggers alarm pheromone release to recruit nestmates, but alone
  at a flower... eh noble but worth it? (Wang & Tan 2019, Introduction)"
        ))
      }
    }
    
    # GIANT BEEEE
    if (encounter == "giant_bee") {
      cat("
  Midmorning: The visitor...

  You are foraging alone on your marked patch.
  Then.... a shadow falls across the flower. A big shadow.

  A. dorsata. A GIANT HONEY BEE. Three times your size.
  It lands on your flower and starts foraging.

  You can detect something in the air... an unfamiliar compound.
  Gamma-octanoic lactone. You have never produced this yourself,
  but you recognise it. It is the alarm pheromone of the giant bees.
  Your antennae are firing.

  The paper calls this inter-species eavesdropping.
  You can read their signal even though it is not meant for you.

  What do you do?
      ")
      
      cat("  [1] Flee! That signal means danger and this bee is huge\n")
      cat("  [2] Release a pheromone and try to defend the patch\n")
      cat("  [3] Stay and forage alongside it... ignore the signal\n")
      cat("  [4] Check your backpack\n")
      
      choice <- pick_option("\n  Your choice: ", 4)
      
      if (choice == 4) {
        show_backpack(backpack)
        cat("\n  Now what?\n")
        cat("  [1] Flee\n")
        cat("  [2] Release a pheromone\n")
        cat("  [3] Stay and forage alongside\n")
        choice <- pick_option("  > ", 3)
      }
      
      if (choice == 1) {
        narrate("
  You leave. The giant bee has the flower.
  You find another patch nearby, smaller, less spectacular.
  But you are alive and your antennae are intact.")
        return(win_phase(
          "You correctly read an inter-species alarm signal and avoided conflict.",
          "A. cerana can detect and respond aversively to gamma-octanoic lactone,
  which is only produced by giant honey bee species (A. dorsata, A. laboriosa).
  This inter-species eavesdropping allows A. cerana to avoid situations
  involving giant bees without direct confrontation.
  (Wang & Tan 2019, Section 2.2)"
        ))
      }
      
      if (choice == 2) {
        pheromone_used <- pick_from_backpack(backpack)
        
        if (pheromone_used == "nothing") {
          choice <- 3
        } else if (pheromone_used == "gamma_octanoic_lactone") {
          narrate("
  You release gamma-octanoic lactone - the giant bee's own signal.
  It pauses. Looks at you. You are a third of its size releasing
  its own keep-out signal at it.
  It is not impressed. It is, if anything, confused.
  It continues foraging. You retreat anyway.")
          return(win_phase(
            "The giant bee was confused by its own signal coming from you.
  It worked, barely, but really you should have just fled.",
            "Gamma-octanoic lactone repels foragers from food sources and is
  used by giant bee species. A. cerana can detect it but producing it
  yourself is not a natural behaviour - fleeing is the ecologically
  correct response to a giant bee. (Wang & Tan 2019, Section 2.2)"
          ))
        } else {
          narrate("
  You release your pheromone at the giant bee.
  It does not care. It is three times your size.
  It continues foraging. You are still here, increasingly
  uncomfortable, releasing bee chemicals at a bee
  that is simply not interested.")
          return(die(
            "You pheromone-signalled at a giant bee until it accidentally
  sat on you. It did not notice.",
            "A. dorsata is significantly larger than A. cerana. Chemical signals
  that work between nestmates or against smaller predators do not
  constitute a viable defence against a giant bee competitor.
  Avoidance is the ecologically appropriate response. (Section 2.2)"
          ))
        }
      }
      
      if (choice == 3) {
        narrate("
  You ignore the gamma-octanoic lactone signal and stay.
  The giant bee, annoyed by your presence, lands on you
  specifically. This was a poor decision.")
        return(die(
          "You ignored an inter-species alarm signal from a bee three times
  your size. It did not end well.",
          "The aversive response of A. cerana to gamma-octanoic lactone is
  adaptive - it prevents costly confrontations with giant bees over
  food sources. Ignoring inter-species chemical signals has real costs.
  (Wang & Tan 2019, Section 2.2)"
        ))
      }
    }
    
    # SPIDER 
    if (encounter == "spider") {
      cat("
  Midmorning: The Ambush!

  You are foraging quietly. 

  On the next flower over, perfectly camouflaged against
  the petals, sits a crab spider.... A sit-and-wait predator.
  It has not seen you yet... Or it has and is pretending it hasn't....
  Those sneaky spiders.

  There is also a dead bee on the petal next to it.
  You can smell old alarm pheromone coming off the body.

  What do you do?
      ")
      
      cat("  [1] Avoid this flower entirely! The dead bee is a warning\n")
      cat("  [2] Land anyway. The nectar is right there and you know it is good\n")
      cat("  [3] Release a pheromone\n")
      cat("  [4] Check your backpack\n")
      
      choice <- pick_option("\n  Your choice: ", 4)
      
      if (choice == 4) {
        show_backpack(backpack)
        cat("\n  Now what?\n")
        cat("  [1] Avoid the flower\n")
        cat("  [2] Land anyway\n")
        cat("  [3] Release a pheromone\n")
        choice <- pick_option("  > ", 3)
      }
      
      if (choice == 1) {
        narrate("
  You avoid the flower. The dead bee was a warning and you read it.
  You find a nearby flower with less nectar but no spider.
  You forage carefully and head home with a modest load.")
        return(win_phase(
          "You correctly used the dead bee as a risk cue and avoided the predator.",
          "Honey bees avoid flowers with dead bee bodies or residual alarm pheromone.
  Dead bodies are honest signals of predation risk at a specific location.
  This avoidance behaviour is ecologically adaptive and well documented.
  (Wang & Tan 2019, Section 3.1)"
        ))
      }
      
      if (choice == 2) {
        narrate("
  You land. The nectar IS right there. You reach for it.
  The spider reaches for you.
  It is faster.")
        return(die(
          "The dead bee was telling you something. You did not listen.
  You are now also telling something to the next bee.",
          "Dead bee bodies near flowers serve as reliable indicators of
  predation risk. Bees that ignore these cues face significantly
  higher predation rates. The dead bee was an honest signal.
  (Wang & Tan 2019, Section 3.1)"
        ))
      }
      
      if (choice == 3) {
        pheromone_used <- pick_from_backpack(backpack)
        
        if (pheromone_used == "nothing") {
          return(die(
            "You released nothing and landed anyway. The spider was grateful.",
            "Without a chemical signal to warn hivemates or deter the predator,
  landing near a sit-and-wait spider is simply dangerous.
  (Wang & Tan 2019, Section 3.1)"
          ))
        }
        
        # Pheromones do not work on them
        narrate(paste0("
  You release your pheromone. The spider regards you with
  all eight of its eyes. It does not speak bee...
  It does not care about bee alarm chemistry...
  It cares about movement and warmth and how tasty you look."))
        return(die(
          "Spiders are not responsive to bee alarm pheromone.
  They are sit-and-wait predators that hunt by vibration and vision,
  not chemical signals. The pheromone did nothing.",
          "Bee alarm pheromones are effective signals within and between
  bee species and some insect predators, BUT crab spiders are
  ambush predators that do not respond to bee chemical signals.
  Avoidance was the only viable option here. (Wang & Tan 2019, Section 3)"
        ))
      }
    }
    
  } # end repeat
}


# ---- PHASE 3: Goin home -----------------------------
# Dendrobium orchid (Section 4.2), Pachysandra flower (Section 4.1) 
# context-dependent learning (Section 4.3)

phase_journey_home <- function(backpack, phase1) {
  
  beetle_risk <- phase1$beetle_risk
  hornet_alerted <- FALSE  # TRUE if player disturbs the orchid...
  
  # Dendrobium orchid 
  repeat {
    
    divider()
    cat("
  Midday: The suspicious orchid... 

  Welp, you're alive and headed home. 

  Ahead of you, a Dendrobium sinense orchid glows white
  in the midday sun. It smells... alarming! Literally.
  Benzyl acetate. Benzyl alcohol. (Z)-11-eicosen-1-ol.
  Your own alarm compounds, coming from a flower??

  And there is a Vespa velutina hornet on it.
  Hovering. Visiting. Apparently pollinating it?
  The orchid is mimicking bee alarm pheromone to attract the hornet.
  The hornet is fooled.

  You have startled it by arriving. It is looking at you.

  What do you do?
    ")
    
    cat("  [1] Fly past immediately! Do NOT interact with the hornet\n")
    cat("  [2] Investigate the orchid: It smells like your own signal!\n")
    cat("  [3] Release a pheromone\n")
    cat("  [4] Check your backpack\n")
    
    choice <- pick_option("\n  Your choice: ", 4)
    
    if (choice == 4) {
      show_backpack(backpack)
      cat("\n  Now what?\n")
      cat("  [1] Fly past\n")
      cat("  [2] Investigate\n")
      cat("  [3] Release a pheromone\n")
      choice <- pick_option("  > ", 3)
    }
    
    if (choice == 1) {
      narrate("
  You fly past without engaging. Good instinct.
  The hornet watches you go. It returns to the orchid.
  You make a mental note: that flower smells like alarm pheromone
  and has a hornet on it. Both of these are bad signs.
  You will tell no one about this flower.")
      win_phase(
        "You avoided the orchid encounter cleanly. Hornet not alerted.",
        "Dendrobium sinense produces compounds including benzyl acetate,
  benzyl alcohol, and (Z)-11-eicosen-1-ol that mimic bee alarm pheromone
  to attract Vespa velutina hornets as pollinators. The orchid benefits
  from this mimicry.
  (Wang & Tan 2019, Section 4.2)"
      )
      break
    }
    
    if (choice == 2) {
      narrate("
  You approach the orchid. It really does smell exactly like
  your alarm pheromone. Fascinating!
  Also... you have now flown directly toward a hornet.")
      hornet_alerted <- TRUE
      win_phase(
        "The hornet has noticed you specifically.... It will follow you home.
  Your day just got more complicated.",
        "Dendrobium sinense mimics bee alarm pheromone to attract hornets
  as pollinators. The five compounds identified (including benzyl acetate
  and (Z)-11-eicosen-1-ol) are electrophysiologically active in hornet
  antennae. Approaching this flower brings you into direct contact
  with a hornet that is already chemically primed. (Section 4.2)"
      )
      break
    }
    
    if (choice == 3) {
      pheromone_used <- pick_from_backpack(backpack)
      
      if (pheromone_used == "nothing") {
        narrate("  You fly past without releasing anything. Safe.")
        win_phase(
          "Clean pass. No complications.",
          "Sometimes no signal is the right signal. (Wang & Tan 2019)"
        )
        break
      }
      
      # Any pheromone alerts the hornet
      narrate(paste0("
  You release your pheromone near the orchid.
  The hornet's antennae fire. Whatever you released, it overlaps
  enough with the alarm chemistry already in the air to get
  the hornet's full attention. It turns toward you.
  Oh boy... your day just got more complicated"))
      hornet_alerted <- TRUE
      win_phase(
        "Your pheromone near the alarm-mimicking orchid alerted the hornet.
  It will be at your hive entrance when you arrive.",
        "The orchid's alarm-mimicking compounds prime the hornet's olfactory
  system. Any additional bee pheromone released nearby adds to a signal
  the hornet is already attending to. You have made yourself a target. A nice, nice target.
  (Wang & Tan 2019, Section 4.2)"
      )
      break
    }
  }
  
  # Pachysandra flower!! 
  repeat {
    
    divider()
    cat("
  Midday: The suspicious flower

  Welp, you're alive and headed home. 
  There are very few flowers left and you know the hive could use more food...
  
  just then you see one more interesting thing...

  A Pachysandra axillaris. A beautiful white flower. The only one around.
  It smells powerfully of benzyl acetate, your own alarm compound?
  Your first instinct is to RUN, but...

  other bees are visiting it? They seem fine....
  The colony needs food. This is the only flower
  you have seen in the last ten minutes.

  What do you do?
    ")
    
    cat("  [1] Avoid it! Alarm smell means danger\n")
    cat("  [2] Watch other bees first then decide... \n")
    cat("  [3] Land immediately! It smells like alarm BUT bees are fine\n")
    cat("  [4] Release a pheromone to warn nestmates away from it\n")
    cat("  [5] Check your backpack\n")
    
    choice <- pick_option("\n  Your choice: ", 5)
    
    if (choice == 5) {
      show_backpack(backpack)
      cat("\n  Now what?\n")
      cat("  [1] Avoid\n")
      cat("  [2] Watch first\n")
      cat("  [3] Land immediately\n")
      cat("  [4] Warn nestmates away\n")
      choice <- pick_option("  > ", 4)
    }
    
    if (choice == 1) {
      narrate("
  You avoid the flower. Alarm smell = danger. 
  You trust your instinct. The colony gets less food.
  You return home empty from this patch.")
      win_phase(
        "You survived. The colony is hungry. The flower was actually fine.",
        "The innate response to alarm pheromone is avoidance. P. axillaris
  exploits this by producing benzyl acetate, BUT bees that overcome
  the innate avoidance through context-dependent learning gain access
  to a reliable food source when others may be unavailable. Avoidance is safe but costly.
  (Wang & Tan 2019, Sections 4.1 and 4.3)"
      )
      break
    }
    
    if (choice == 2) {
      narrate("
  You hover and watch. Three bees land. Forage. and leave.
  Unharmed. No predators. No ant ambushes. Just goodness. 
  The flower smells alarming... but doesn't SEEM alarming.
  You land. Context-dependent learning in action!
  You fill your baskets. The hive will be happy.")
      win_phase(
        "Context-dependent learning. You gained information and gained food.",
        "Bees can learn to associate alarm pheromone compounds with food reward
  when experienced in an appetitive context. Observing nestmates foraging
  safely provides exactly this context. This explains why plants mimicking
  alarm pheromone can still attract pollinators, and why benzyl acetate
  appears in 38 plant families! (Wang & Tan 2019, Section 4.3)"
      )
      break
    }
    
    if (choice == 3) {
      narrate("
  You land confidently and the forage is awesome. 
  You were mildly deceived by a plant...
  but landed anyways.
  You collect and continue home.")
      win_phase(
        "Bold and rewarded. The plant's mimicry worked on you and got your attention!",
        "P. axillaris produces benzyl acetate as 95% of its floral volatile,
  the same compound that is the primary alarm signal of A. cerana.
  The plant benefits from attracting bees despite signalling danger.
  The bee benefits from food when other sources may be scarce...
  An odd but functional relationship.
  (Wang & Tan 2019, Section 4.1)"
      )
      break
    }
    
    if (choice == 4) {
      pheromone_used <- pick_from_backpack(backpack)
      
      if (pheromone_used == "nothing") {
        narrate("  You decide not to release anything and fly past.")
        win_phase(
          "You passed the flower without interacting. Colony misses the food.",
          "Avoidance without signalling is safe but means the colony loses
  access to a food source. (Wang & Tan 2019, Section 4.1)"
        )
        break
      }
      
      if (pheromone_used %in% c("isopentyl_acetate", "benzyl_acetate")) {
        narrate("
  You release alarm pheromone near the flower.
  Foragers in the area immediately veer away from it.
  One drops a full forage sack in confusion.
  You have successfully destroyed your colony's access
  to a great food source.
  Well done.")
        if (pheromone_used == "isopentyl_acetate") beetle_risk <- TRUE
        win_phase(
          "You warned nestmates away from a perfectly safe food source.
  The colony will be hungry. The flower was fine.",
          "You smelled danger, and passed on that signal without looking around.
  P. axillaris, the flower, produces this bee alarm chemical to ATTRACT you. 
  Bees that overcome the innate avoidance through context-dependent learning gain access
  to a reliable food source when others may be unavailable.
  (Wang & Tan 2019, Table 1, Section 4.1)"
        )
        break
      } else {
        narrate("
  You release your pheromone near the flower.
  It does not strongly repel nestmates from this location.
  They continue visiting the flower.
  You have achieved nothing except releasing a compound
  at a flower that was trying to attract you.")
        win_phase(
          "No major consequence this time. The flower and your hivemates are fine.",
          "Not all pheromone compounds repel foragers at flowers. The specific
  compounds that do this are isopentyl acetate and benzyl acetate.
  Other compounds have different functions. (Wang & Tan 2019, Table 1)"
        )
        break
      }
    }
    
  } 
  
  return(list(
    backpack        = backpack,
    hornet_alerted  = hornet_alerted,
    beetle_risk     = beetle_risk
  ))
}


# ---- PHASE 4: Back at the hive... -------------------------
# I-see-you signal, van der Vecht gland marking,
# heat ball chemistry, hornet alarm pheromone compounds

phase_hive_entrance <- function(backpack, phase3) {
  
  hornet_alerted <- phase3$hornet_alerted
  beetle_risk    <- phase3$beetle_risk
  hive_marked    <- FALSE
  
  repeat {
    
    divider()
    
    if (hornet_alerted) {
      cat("
  Afternoon: Back at the hive...

  You arrive at the hive. The hornet from earlier followed you.
  It is not scouting. It is here because of YOU!
  You spot it hovering at the entrance of your hive...
      ")
      
    } else {
      cat("
  Afternoon: Back at the hive...

  You arrive at the hive. There is a Vespa velutina hornet
  hovering at the entrance. It is scouting. It has not attacked
  and has also not marked your hive yet as a good source...
      ")
    }
    
    cat("  [1] Perform the I-see-you abdomen shaking signal\n")
    cat("  [2] Release a pheromone from your backpack\n")
    cat("  [3] Fly directly at the hornet\n")
    cat("  [4] Sneak into the hive... hope it does not notice\n")
    cat("  [5] Check your backpack\n")
    
    choice <- pick_option("\n  Your choice: ", 5)
    
    if (choice == 5) {
      show_backpack(backpack)
      cat("\n  Now what?\n")
      cat("  [1] I-see-you signal\n")
      cat("  [2] Release a pheromone\n")
      cat("  [3] Fly at the hornet\n")
      cat("  [4] Sneak inside\n")
      choice <- pick_option("  > ", 4)
    }
    
    if (choice == 1) {
      if (hornet_alerted) {
        narrate("
  You perform the I-see-you signal. Rapid abdomen shaking commence! 
  The hornet hesitates. This signal says: I see you. We see you.
  The hornet understands, but it wants you. It is not easily deterred.
  It slows but does not leave.")
      } else {
        narrate("
  You perform the I-see-you signal. Rapid abdomen shaking commence! 
  The hornet hesitates. It knows what that means... It slows its hover.")
      }
      
      cat("\n  The hornet is slowed but not gone. Do you add a pheromone signal?\n")
      cat("  [1] Yes! Release a pheromone now!\n")
      cat("  [2] No. Maintain the I-see-you signal only. Keep shaking that BEEhind\n")
      choice2 <- pick_option("  > ", 2)
      
      if (choice2 == 2) {
        if (hornet_alerted) {
          narrate("
  The I-see-you signal alone is not enough for a hornet that
  followed you home with intent. It marks the hive entrance
  with van der Vecht gland chemicals and leaves.")
          hive_marked <- TRUE
          win_phase(
            "The signal alone was not enough. The hive has been marked.
  More hornets will come tonight.... things are not looking good.",
            "The I-see-you signal is effective against scouting hornets but
  an already-agitated hornet requires stronger deterrence.
  The van der Vecht gland marking recruits additional hornets to the
  location. This is bad for your colony...(Section 3.2)"
          )
        } else {
          narrate("
  The hornet hovers for a long time. Eventually it drifts away.
  Uncertain. Not worth the risk today. It knows you saw it scouting. Phew!")
          win_phase(
            "The I-see-you signal alone worked this time. The hive is safe!",
            "The I-see-you abdomen shaking signal in A. cerana evolved specifically
  to repel Vespa velutina hornets at the hive entrance. It is an honest
  signal that communicates colony awareness and readiness to defend.
  (Wang & Tan 2019, Section 3.2)"
          )
        }
        break
      }
      
      # Add pheromone to the dance
      pheromone_used <- pick_from_backpack(backpack)
      
      if (pheromone_used == "nothing") {
        if (hornet_alerted) {
          hive_marked <- TRUE
          narrate("  The hornet marks the hive. You stood there and watched... oh boy.")
          win_phase(
            "Hive marked by the scout. More hornets will come tonight.",
            "Without chemical backup, the I-see-you signal was insufficient
  against an already-agitated hornet. (Section 3.2)"
          )
        } else {
          win_phase(
            "Your dance worked on a scouting hornet!",
            "The I-see-you signal deters scouting hornets effectively. 
            Not worth the risk if they think you saw them. (Wang & Tan 2019, Section 3.2)"
          )
        }
        break
      }
      
      if (pheromone_used %in% c("isopentyl_acetate", "benzyl_acetate",
                                "hexyl_acetate", "z11_eicosen_1_ol")) {
        if (pheromone_used == "isopentyl_acetate") beetle_risk <- TRUE
        narrate("
  The I-see-you signal combined with alarm pheromone pouring
  from the entrance is A CALL TO ARMS! Guards swarm out.
  The hornet retreats. Guards locate and remove the partial
  van der Vecht marking from the entrance. Chemical warfare, countered.")
        win_phase(
          "Perfect defence. your colony is safe... for now.",
          "A. cerana combines the I-see-you behavioural signal with alarm pheromone
  recruitment for maximum effectiveness against hornets. Guards can detect
  and remove van der Vecht gland markings, preventing further recruitment
  of hornets to the hive location. (Wang & Tan 2019, Section 3.2)"
        )
        break
        
      } else if (pheromone_used %in% c("two_heptanone", "gamma_octanoic_lactone")) {
        narrate("
  You release your pheromone. Guards begin avoiding the entrance area.
  The hornet, now unimpeded, marks the hive.")
        hive_marked <- TRUE
        win_phase(
          "Wrong compound. Guards avoided the entrance. The hornet could mark the hive effectively...",
          "Using avoidance or repellent compounds at the hive entrance drives
  your own guards away rather than recruiting them. Compound selection
  is critical in defensive contexts. Had you used a recruitment alarm, others may have come
          and helped to fend off the marking by the hornet. (Wang & Tan 2019, Table 1)"
        )
        break
      }
    }
    
    if (choice == 2) {
      pheromone_used <- pick_from_backpack(backpack)
      
      if (pheromone_used == "nothing") {
        narrate("  You release nothing. The hornet marks the hive.")
        hive_marked <- TRUE
        win_phase(
          "You sat there... the hornet came forward and marked the hive. More will come.",
          "Failing to respond to a scouting hornet allows it to mark the hive
  with van der Vecht gland chemicals, recruiting additional hornets. This is not good for you.
  (Wang & Tan 2019, Section 3.2)"
        )
        break
      }
      
      if (pheromone_used %in% c("isopentyl_acetate", "benzyl_acetate",
                                "hexyl_acetate", "z11_eicosen_1_ol")) {
        if (pheromone_used == "isopentyl_acetate") beetle_risk <- TRUE
        narrate("
  The hornet sneaks into your hive, but your pheromone alerted others to the intrusion.
  Guards rush towards you and the hornet. 
  THey form a heat ball around the hornet.
  Flight muscles vibrate. Temperature rises to 46 degrees C.
  The hornet is baked alive inside a ball of bees.
  It worked well, but was also energetically expensive for the colony.")
        win_phase(
          "Heat ball success! Hornet dead.",
          "During heat ball formation both bees and hornets release alarm
  pheromones simultaneously. The specific compounds have been identified:
  isopentyl acetate, octyl acetate, benzyl acetate from bees; 2-heptanone,
  2-nonanone, 2-undecanone from the hornet. The heat ball raises temperature
  to 46 degrees C, which is lethal to the hornet but survivable by the bees.
  (Wang & Tan 2019, Section 3.2)"
        )
        break
        
      } else if (pheromone_used %in% c("two_heptanone", "gamma_octanoic_lactone")) {
        narrate("
  Guards move away from the entrance. The hornet marks it uncontested.")
        hive_marked <- TRUE
        win_phase(
          "Wrong compound use cleared the way for the hornet.
          Your hive is marked and more hornets will now come...",
          "Avoidance signals at the hive entrance drive defenders away.
  (Wang & Tan 2019, Table 1)"
        )
        break
      }
    }
    
    if (choice == 3) {
      return(die(
        "You flew directly at a hornet three times your size.
  Everyone admired your commitment. Honorable attampt....",
        "A single bee cannot overpower a hornet. Collective defence via
  alarm pheromone recruitment and heat ball formation is the effective
  strategy. Individual heroics are ecologically inefficient. (Section 3.2)"
      ))
    }
    
    if (choice == 4) {
      narrate("
  You slip inside. The hornet marks the hive entrance
  with van der Vecht gland chemicals.
  Later you detect the marking from inside...
              smells funny.")
      hive_marked <- TRUE
      
      cat("
  You can smell the van der Vecht marking from inside.
  You know what it means. (The hornet marked your hive and it's friends will be back.
  Do you do anything about it?

  [1] Go back out and try to remove it\n")
      cat("  [2] Someone else will deal with it\n")
      mark_choice <- pick_option("  > ", 2)
      
      if (mark_choice == 1) {
        narrate("
  You go back out. You locate the chemical marking on the
  entrance and work to remove it.
  Other guards join you and the marking is cleared.
  The hornets will not find you now!")
        hive_marked <- FALSE
        win_phase(
          "You removed the van der Vecht marking. The hive is no longer flagged.",
          "Honey bees can detect and remove van der Vecht gland markings left
  by scout hornets. This chemical counter-measure prevents the scout's
  marking from recruiting additional hornets to the hive location. (Section 3.2)"
        )
      } else {
        win_phase(
          "The marking stays. More hornets are coming tonight.",
          "Van der Vecht gland chemicals from hornets recruit additional hornets
  to a marked hive location. Ignoring the marking has serious consequences
  for the colony. How lazy... how detrimental. (Wang & Tan 2019, Section 3.2)"
        )
      }
      break
    }
    
  } 
  
  return(list(
    backpack     = backpack,
    beetle_risk  = beetle_risk,
    hive_marked  = hive_marked
  ))
}


# ---- PHASE 5: The end... dun dun DUNNNN ----------------------
# small hive beetle attracted by isopentyl acetate,
# the inevitable wax moth. Yay death for all

phase_inside_hive <- function(backpack, phase4) {
  
  beetle_risk <- phase4$beetle_risk
  hive_marked <- phase4$hive_marked
  
  divider()
  cat("
  Evening: Inside the hive 

  You made it. You are home.
  Take a deep breath. 
  ")
  press_enter()
  
  # Beetle : heh heh 
  if (beetle_risk) {
    divider()
    cat("
  Wait a minute... 

  There is a small hive beetle, Aethina tumida, at the entrance.
  Actually there are several....

  Here is the thing: isopentyl acetate, the best-selling pheromone you used today, attracts them.
  Low doses of alarm pheromone at the hive entrance lead to high
  electrophysiological responses in the beetle. Isopentyl acetate
  alone is sufficient to attract them....

  You released this pheromone today... this is a consequence of that.

  The colony is trying to deal with the beetles. Guard bees
  are attempting to corral them.
  The beetles are slipping through gaps.

  What do you do?
    ")
    
    cat("  [1] Release alarm pheromone: recruit more guards\n")
    cat("  [2] Join the physical defence: push them out manually\n")
    cat("  [3] Do nothing... the guards have it\n")
    cat("  [4] Check your backpack\n")
    
    choice <- pick_option("\n  Your choice: ", 4)
    
    if (choice == 4) {
      show_backpack(backpack)
      cat("\n  Now what?\n")
      cat("  [1] Release alarm pheromone\n")
      cat("  [2] Physical defence\n")
      cat("  [3] Do nothing\n")
      choice <- pick_option("  > ", 3)
    }
    
    if (choice == 1) {
      pheromone_used <- pick_from_backpack(backpack)
      if (pheromone_used == "isopentyl_acetate") {
        narrate("
  You release isopentyl acetate to recruit more guards.
  More guards come. More beetles also come.
  What were you thinking??
  The beetles are specifically attracted to this compound!
  You have made the infestation worse.")
        win_phase(
          "Releasing isopentyl acetate attracted more beetles.",
          "Isopentyl acetate alone attracts small hive beetles to the hive entrance. 
          Releasing more of it when beetles are already present recruits 
          additional beetles along with the guard bees. 
          This is a documented and specific problem with
  over-reliance on your favorite pheromone. (Wang & Tan 2019, Section 3.3)"
        )
      } else if (pheromone_used %in% c("hexyl_acetate", "z11_eicosen_1_ol",
                                       "benzyl_acetate")) {
        narrate("
  You release your pheromone. Guards are recruited.
  The beetles are not specifically attracted to this compound.")
        win_phase(
          "Smart choice. You recruited guards without attracting more beetles.",
          "Not all alarm compounds attract small hive beetles equally.
  Isopentyl acetate is the specific attractant. Using other recruitment
  compounds avoids compounding the beetle problem. (Section 3.3)"
        )
      } else {
        narrate("
  Your pheromone does not help much here. The beetles get in.")
        win_phase(
          "The beetles got in. The colony will deal with it.",
          "Small hive beetles are a significant parasite of bee colonies.
  (Wang & Tan 2019, Section 3.3)"
        )
      }
    }
    
    if (choice == 2) {
      narrate("
  You join the physical defence. You push beetles away from
  the entrance. Some beetles get in anyway.
  The colony will survive this, but take significant damage.")
      win_phase(
        "Physical defence helped, but some beetles got through.",
        "Honey bee colonies physically corral small hive beetles into
  propolis prisons when they cannot be expelled. (Wang & Tan 2019, Section 3.3)"
      )
    }
    
    if (choice == 3) {
      narrate("
  The guards are overwhelmed.
  The colony will deal with a significant infestation tonight.")
      win_phase(
        "The beetles establishe and the colony is alive, but hurting...",
        "Small hive beetles are important parasites at both individual and
  colony level. They are attracted to hive alarm pheromones, meaning
  the bees' own defence signals can inadvertently recruit their parasites.
  (Wang & Tan 2019, Section 3.3)"
      )
    }
  }
  
  # HIVE MARKED
  if (hive_marked) {
    divider()
    cat("
  Oh no.... I hear hornets...

  The van der Vecht marking did its job.
  As the sun sets, three more Vespa velutina arrive at the entrance.
  The colony goes into full defensive mode.
  Two more heat balls form. It is expensive. Chaotic.
  Several bees are lost. The hive survives but barely.
  
  If only you had cleaned up that signal...
  your friends may still be alive...
  ")
    press_enter()
  }
  
  # Death to all... the wax moth... sorry 
  divider()
  cat("
  Night falls... you have survived...

  The hive is quiet. The day is done.
  You are tired and drift off to sleep.

  At 2am, something moves at the entrance.

  A wax moth. 
  
  Just your luck, it has chosen your hive tonight. 
  Its antennae detect your alarm pheromone perfectly.
  Electrophysiologically it is firing on all channels.
  It can smell everything you released today.

  It does not care.

  It invades at night because your guards are slow and few.
  The alarm pheromone is not a credible threat at 2am.

  You release alarm pheromone. Your nestmates stir groggily.
  The moth is already inside.

  You release more. It does not matter.
  The moth saunters over to you... 
  and...
  
  *DEATH*
  ")
  press_enter()
  
  divider()
  cat("
  *** YOU DIED ***

  Not from the ants, or the hornet, or the flower, or the beetle.
  From a moth at 2am that couldn't care less about your pheromones from BEEtrice.

  Your sister, who made completely different choices today, also died.
  
  As is life.
  
  The world is complicated.
  The moth always comes at night.

  ~~ THE END ~~
  
  Type  bee_adventure()  to play again.
  ")
  press_enter()
}

# ---- MAIN GAME ---------------------------------------------

bee_adventure <- function() {
  
  divider()
  cat("
  TO BEE OR NOT TO BEE
  A Pheromone Survival Game
  Based on: Wang & Tan (2019), Insects 10(10): 336
  Ceili DeMarais, ENTO 510

  You are Apis cerana. An eastern honey bee. Tasty.
  Your goal: contribute to your hive... and... survive the day.
  
  A day in the life of a bee is challenging. You will encounter many obstacles,
  and many many choices. Choose wisely. Death awaits. 

  If you die during a phase, that phase restarts.

  Good luck. Buzz on. 
  ")
  press_enter()
  
  # Visit Beetrice once at the start only
  backpack <- visit_perfumary()
  
  # Forage
  p1 <- NULL
  repeat {
    p1 <- phase_flower_patch(backpack)
    if (!is.null(p1) && !isFALSE(p1)) break
    cat("\n  Restarting Phase 1...\n")
    press_enter()
  }
  
  # Attack
  p2 <- NULL
  repeat {
    p2 <- phase_something_arrives(p1$backpack, p1)
    if (!is.null(p2) && !isFALSE(p2)) break
    cat("\n  Restarting Phase 2...\n")
    press_enter()
  }
  
  # Goin home
  p3 <- NULL
  repeat {
    p3 <- phase_journey_home(p1$backpack, p1)
    if (!is.null(p3) && !isFALSE(p3)) break
    cat("\n  Restarting Phase 3...\n")
    press_enter()
  }
  
  # Hornet
  p4 <- NULL
  repeat {
    p4 <- phase_hive_entrance(p3$backpack, p3)
    if (!is.null(p4) && !isFALSE(p4)) break
    cat("\n  Restarting Phase 4...\n")
    press_enter()
  }
  
  # Beetles and always ends with Moth 
  phase_inside_hive(p4$backpack, p4)
  
  divider()
  cat("  Type  bee_adventure()  to play again.\n\n")
}

# run it run it run t
bee_adventure()
