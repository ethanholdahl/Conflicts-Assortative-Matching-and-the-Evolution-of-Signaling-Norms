

## Load and install the packages
library("tidyverse", "shiny")
theme_set(theme_minimal())


# Define UI for the application
navbarPage(title = "Costly Singals",
           tabPanel(
             "Speed of Evolution",
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
                 "Imagine a game where there are two types of players: high types (H) and low types (L).
                               In every period players are randomly matched with another player. High types get a higher
                               payoff when matched with high types than when they match with low types and high types get
                               a higher payoff when matched with low types than low types get when matched with low types.
                               So we have the relation:
                               $$V(H,H) > V(H,L) > V(L,L)$$
                               Clearly, high type (H) is the dominant type and without signaling H evolves."
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
                 "Imagine now that players can signal and the cost of signaling is K. 
                 I will only consider cases where the signal is potentially beneficial for the high types.
                 This requires that the payoff of matching with a high type minus the cost of signaling is
                 greater than the payoff of matching with a low type:
                 $$V(H,H) - K > V(H,L)$$
                 The potential benefit of signaling to guarentee matching with a high type is contingent of the signal being an honest one,
                 that is to say that low types are better off matching with another low type than paying the cost to match with a high type:
                 $$V(L,L) > V(L,H) - K$$
                 Thus, if we combine the above two inequalities we know that for a signal of cost K to be viable it must be the case that:
                 $$V(H,H) + V(L,L) > V(L,H) + V(H,L)$$
                 Note that whether there is a signal or not high types (H) recieve higher payoffs than low type and the population evolves to high types (H) over time.
                 However, since signaling potentially increases the payoff to high types the speed of evolution under signaling is different than it is under no signaling.
                 I will analyze this phenomena in three scenarios all compared to the baseline of no signaling:",
                 br(),
                 br(),
                 "1. Signals are temporary and elective",
                 br(),
                 "2. Singals are permanent and elective",
                 br(),
                 "3. Signals are permanent and appointed"
               ),
               br(),
               tabsetPanel(
                 tabPanel("Temporary and Elective",
                          helpText("g")),
                 tabPanel("Permanent and Elective",
                          helpText("g")),
                 tabPanel("Permanent and Appointed",
                          helpText("g"))
               )
             )
           ))