# PWHL Goalie Tracker 🥅

## Motivation

**Target Audience:** Hockey fans!

The Professional Women's Hockey League (PWHL) launched its inaugural season in January 2024. First of its kind, it brings together world-ranking players, to compete among six teams located in North America. With high ticket sales and media coverage from the start, the fan support has been impressive, marking a historic moment in women’s sports in general. While this app serves to further promote the PWHL, it shines a spotlight on one of the most crucial, and sometimes overlooked, aspects of the game: goalies! 

By providing a convenient way to interact with goalie statistics, like save percentage and average goals against, along with overall team rankings, users can dive into the depths of the PWHL.  Beyond enhancing the fan experience, this app recognizes the pivotal role of sports statistics in informing coaching decisions and even facilitating engagement in sports betting. 

## App Description

## Installation Instructions 

To run the dashboard locally follow the steps below. 

1. Clone the repository. In your terminal run:

    ```console
    $ git clone https://github.ubc.ca/MDS-2023-24/DSCI_532_individual-assignment_nbidwell.git
    ```

2. In R or a RStudio console, install `renv` if you don't have it: 

    ```console
    $ install.packages("renv")
    ```
3. Install the package dependencies in the `renv.lock` file: 

    ```console
    $ renv::restore()
    ```

3. To run the dashboard, from the root of the directory (`DSCI_532_individual-assignment_nbidwell`) run: 

    ```console
    $ shiny::runApp("R")
    ```

4. Click on the displayed link to open the dashboard on a browser.


## License

PWHL Goalie Tracker is licensed under the MIT license. See [LICENSE.md](LICENSE.md) for details. 

## Attribution

#### 📈 Data Source

Data for PWHL Goalie Tracker was sourced through the use of [fastRhockey: The SportsDataverse's R Package for Hockey Data](ttps://fastRhockey.sportsdataverse.org), developed by Ben Howell and Saiem Gilani. The [GitHub released version](https://github.com/sportsdataverse/fastRhockey) was used. 