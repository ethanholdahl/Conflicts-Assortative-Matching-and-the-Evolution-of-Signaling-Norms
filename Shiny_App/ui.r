## Load and install the packages
library("tidyverse", "shiny")
theme_set(theme_minimal())


# Define UI for the application
navbarPage(
  title = "Costly Singals",
  theme = shinythemes::shinytheme("spacelab"),
  navbarMenu(
    "Speed of Evolution",
    tabPanel(
      "Binay Quality",
      tags$body(
        withMathJax(),
        h2("High Type - Low Type Model"),
        h3("No Signaling"),
        HTML(
          '<div align="center">
                        <table border ="1" style="width:50%">
                        <tr>
                        <th></th>
                        <th style="text-align:center"> H </th>
                        <th style="text-align:center"> L </th>
                        </tr>
                        <tr>
                        <th style="text-align:center"> H </th>
                        <td style="text-align:center"> V(H,H) </td>
                        <td style="text-align:center"> V(H,L) </td>
                        </tr>
                        <tr>
                        <th style="text-align:center"> L </th>
                        <td style="text-align:center"> V(L,H) </td>
                        <td style="text-align:center"> V(L,L) </td>
                        </tr>
                        </table>
                        </div>'
        ),
        helpText(
          "We are interested in the effect that signaling has on the evolution of a population so we consider two populations:
                   one with signaling technology and the other without signaling technology. Both populations are endowed with hightype
                   and low type individuals. We use a discrete model where in each period individuals match with another individual in
                   their population. When an individual is not signaling they randomly match with another non-signaling individual in
                   their population. However, when an individual does signal they randomly match with another signaling individual in
                   their population. After matching, they reproduce according to the payoffs in the table below minus the cost of
                   signaling if applicable. This is their reproductive rate.  Since payoffs are symmetric we only present the payoffs
                   for the row player.We want to set up the payoffs such that high types would evolve even in absence of signaling i.e.
                   survival of the fittest. Thus, the relation:
                               $$V(H,H)>V(H,L), V(L,H)>V(L,L)$$
                               is required. This relation tells us that one’s reproductive rate is increasing both in one’s type and in their mate’s type.

                   In a population without signaling individuals can not observe the type of those they match with. As such they are
                   randomly matched with another individual in their population. We define the non-signaling population at time \\(t\\) as
                   \\(N_t\\) where \\(N_t = N^H_t + N^L_t\\). Here, \\(N^H_t\\) and \\(N^L_t\\) are the amount of high types and low types, respectively,
                   in the non-signaling population at time \\(t\\).
                   Assuming a large \\(N_t\\), the law of large numbers implies that \\(N^H_t\\) and \\(N^L_t\\) evolve according to their expected payoffs
                   from the above payoff table. So we have:

    $$ N^H_{t+1}=[N^H_t / N_t*V(H,H)+N^L_t / N_t*V(H,L)]*N^H_t$$
    $$ N^L_{t+1}=[N^H_t / N_t*V(L,H)+N^L_t / N_t*V(L,L)]*N^L_t$$"
        ),
        h3("Signaling with cost K"),
        HTML(
          '<div align="center">
                        <table border ="1" style="width:50%">
                        <tr>
                        <th></th>
                        <th style="text-align:center"> H </th>
                        <th style="text-align:center"> L </th>
                        </tr>
                        <tr>
                        <th style="text-align:center"> H </th>
                        <td style="text-align:center"> V(H,H) - K </td>
                        <td style="text-align:center"> V(H,L) - K </td>
                        </tr>
                        <tr>
                        <th style="text-align:center"> L </th>
                        <td style="text-align:center"> V(L,H) - K </td>
                        <td style="text-align:center"> V(L,L) - K </td>
                        </tr>
                        </table>
                        </div>'
        ),
        helpText(
          "Here we consider a population that does have signaling technology. Let \\(K\\) be the cost of signaling. Consistent with the handicap principle
          we assume that the cost of the signal, \\(K\\), is sufficiently costly such that that only high types can afford it. Formally, we require:
                   $$V(L,L)>V(L,H)-K$$
                   We also only want to consider signals that are potentially incentive compatible for high types. That is that high types receive a higher
                   payoff by matching with another high type even after paying the cost of the signal than they do by matching with a low type:
                   $$V(H,H)-K>V(H,L)$$
                   Combining the two inequalities, we know that for a viable signal of cost $K$ to exist it must be the case that:
                   $$V(H,H)+V(L,L)>V(L,H)+V(H,L)$$
                   This is known as the single crossing property.
                   We enforce these conditions in the selection of the reproductive rates $V(.,.)$ and cost of signal $K$. Since we are interested in analyzing
                   equilibrium effects we assume that individuals in the population with signaling technology follow the incentive compatibility conditions above
                   (3,4), that is that only high types signal and only low types do not signal.
                   We define the population with signaling at time \\(t\\) as \\(S_t\\) where \\(S_t = S^H_t + S^L_t\\). Here, \\(S^H_t\\) and \\(S^L_t\\) are
                   the amount of high types and low types, respectively, in the signaling population at time \\(t\\).
                   Because of signaling, \\(S^H_t\\) and \\(S^L_t\\) only match with their own type and thus experience a constant growth rate over time. Their
                   evolutionary dynamics are as follows:
                   $$S^H_{t+1}=[V(H,H)-K]*S^H_t$$
          $$S^L_{t+1}=V(L,L)*S^L_t$$"
          ),
        h3("Competition Between Populations"),
        helpText(
          "Assume that in period \\(T\\) the non-signaling population and the signaling population engage in competition. In periods \\(t \\in [0,T)\\) the
        populations evolved according to the dynamics in the above sections, but starting at period \\(T\\) they start to eliminate the other population
        according to Lanchester's square law. In each period, before reproduction, each member of a population kills \\(\\beta\\) members of the other population,
          independent of their type. So the dynamics for \\(N^H, N^L, S^H,\\) and \\(S^L\\) are now:
          $$N^H_{t+1}=[N^H_t / N_t*V(H,H)+N^L_t / N_t*V(H,L)]*max\\{[N^H_t-\\beta N^H_t / N_t * I_{t \\geq T}S_t],0\\}$$
          $$N^L_{t+1}=[N^H_t / N_t*V(L,H)+N^L_t / N_t*V(L,L)]*max\\{[N^L_t-\\beta N^L_t / N_t * I_{t \\geq T}S_t],0\\}$$
          
          $$S^H_{t+1}=[V(H,H)-K]*max\\{[S^H_t-\\beta S^H_t / S_t * I_{t \\geq T}N_t],0\\}$$
          $$S^L_{t+1}=V(L,L)*max\\{[S^L_t-\\beta S^L_t / S_t * I_{t \\geq T}N_t],0\\}$$
          
          In the equations above, \\(I_{t \\geq T}\\) is an indicator that the groups are engaging in group competition and the max argument simply ensures that
          the populations don't reach a negative number."
        ),
        helpText(
          br(),
          br(),
          "In the simulations below you will be able to select values for a few parameters.
                 These include:",
          br(),
          br(),
          "1. Values for \\(V(H,H)\\), \\(V(H,L)\\), \\(V(L,L)\\), and \\(V(L,H)\\)",
          br(),
          "2. \\(H_0 \\in [0,1] \\) : The initial % of high type (H) players",
          br(),
          "3. The cost of signaling \\(K\\)",
          br(),
          "4. Time horizon \\(t\\)",
          br(),
          "5. (If applicable) \\(H_{0,S}, L_{0,S}\\): The initial proportion of high and low type signaling",
          br(),
          "6. How the size of the population evolves (Not all options yet operational)",
          br(),
          "7. How populations interact when they meet",
          br(),
          "8. The level of agression in competition: \\(\\beta\\)",
          br(),
          "9. When competition starts: \\(T\\)"
        ),
        br(),
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              "vHH",
              label = "V(H,H):",
              min = 1,
              max = 3,
              step = .01,
              value = 1.15
            ),
            sliderInput(
              "vHL",
              label = "V(H,L):",
              min = 0,
              max = 3,
              step = .01,
              value = .9
            ),
            sliderInput(
              "vLL",
              label = "V(L,L):",
              min = 0,
              max = 3,
              step = .01,
              value = .8
            ),
            sliderInput(
              "vLH",
              label = "V(L,H):",
              min = 0,
              max = 3,
              step = .01,
              value = .85
            ),
            sliderInput(
              "ratio",
              label = "Initial high type \\(H_0\\):",
              min = 0,
              max = 1,
              step = .01,
              value = .2
            ),
            sliderInput(
              "K",
              label = "The cost of signaling \\(K\\)",
              min = 0,
              max = 2,
              step = .01,
              value = .1
            ),
            sliderInput(
              "time",
              label = "Time horizon \\(t\\):",
              min = 1,
              max = 200,
              step = 1,
              value = 30
            ),
            sliderInput(
              "ratio_h",
              label = "Initial % signalers given high type \\(H_{0,S}\\):",
              min = 0,
              max = 1,
              step = .01,
              value = .5
            ),
            sliderInput(
              "ratio_l",
              label = "Initial % signalers given low type \\(L_{0,S}\\):",
              min = 0,
              max = 1,
              step = .01,
              value = 0
            ),
            radioButtons(
              "pop_grow",
              label = "Select how the size of the population evolves",
              choices = c(
                "Fixed population",
                "Unbounded exponential growth",
                "Logistic growth to capacity"
              ),
              selected = "Unbounded exponential growth"
            ),
            radioButtons(
              "join_scenario",
              label = "When populations join together do they fight or merge?",
              choices = c("Fight", "Merge"),
              selected = "Fight"
            ),
            sliderInput(
              "beta",
              label = "\\(\\beta\\): The number each member kills each period when fighting",
              min = 0,
              max = 1,
              step = .01,
              value = .2
            ),
            sliderInput(
              "start",
              label = "\\(T\\): The start of competition",
              min = 1,
              max = 200,
              step = 1,
              value = 20
            )
          ),
          mainPanel(
            h4("Population Dynamics"),
            helpText(
              "Within population groups evolve according to the
                            growth scenario selected. Note: Logistic not yet operational."
            ),
            tabsetPanel(
              tabPanel(
                "One population, signaling is temporary and elective",
                plotOutput("TE"),
                helpText(
                  "In this scenario, at the start of each period individual decide
                                     as a group if they would like to signal or not. This represents optimal behavior.
                                         If individuals have no coordination at all the high types would never stop signaling."
                )
              ),
              tabPanel(
                "One populaton, signaling is permanent and appointed",
                plotOutput("PA_P"),
                plotOutput("PA_R"),
                plotOutput("PA_G"),
                helpText(
                  "In this scenario, both type and signal
                                     (or lack thereof) are retained between
                                     each period with the initial type and signal levels
                                     determined by the inputs on the left."
                )
              ),
              tabPanel(
                "Two separate populations: One where everyone signals, the other where no one signals",
                plotOutput("Sep_P"),
                plotOutput("Sep_R"),
                plotOutput("Sep_G"),
                helpText(
                  "Here the signal reduces the payoffs to both the low and high types.
                                   However, since the cost of the signal is flat and high types have higher payoffs than low types,
                                   the low types are hurt more than the high types by the signal. This means that high types evolve faster under a signal. This can result in periods
                                   where the signaling population experiencs higher payoffs than the non-signaling population. However, as high types
                                   evolve in both populations there comes a point where the greater proportion of high types in the signaling
                                   population doesn't make up for the cost of the signaling meaning the no signaling population experiences higher payoffs.
                                       As a result, most paramaterizations result in non-signal populations initially experiencing higher payoffs (since the proportion of high types start at the same level),
                                       followed by a period where the signaling population experiences higher payoffs due to the larger proportion of high types,
                                       ending with the non-signaling population surpassing the signaling population as the evolution of that group catches up."
                )
              ),
              tabPanel(
                "Two separate populations: One where everyone signals, the other where no one signals. They join to form one population at period t/2.",
                plotOutput("Join_P"),
                plotOutput("Join_R"),
                plotOutput("Join_G"),
                helpText(
                  "Here I reduce each group to half it's size when the groups join at period t/2 to maintain the a fixed population size."
                )
              ),
              tabPanel(
                "Two spereate populations: One where only high types signal, the other where no one signals",
                plotOutput("SepH_P"),
                plotOutput("SepH_R"),
                plotOutput("SepH_G"),
                helpText("Add Text Here")
              ),
              tabPanel(
                "Two spereate populations: One where only high types signal, the other where no one signals. After a period of peace they engage in combat and continue to fight until only one population remains.",
                plotOutput("JoinH_P"),
                plotOutput("JoinH_R"),
                plotOutput("JoinH_G"),
                plotOutput("KTregion"),
                plotOutput("BTregion"),
                plotOutput("KBregion"),
                helpText(
                  "The veritcal line in the top trio of graphs indactes the last period of peace before fighting starts. In the bottom trio of graphs, the white intersecting lines indicate the current paramater values. The different regions indicate which population, the signaling or non-signaling population would beat the other population at those paramater values."
                )
              )
            )
          )
        )
      ),
      tabPanel("Discrete Quality"),
      tabPanel("Continuous Quality")
    )
  )
)
