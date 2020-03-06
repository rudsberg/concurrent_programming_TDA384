package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
        extends SequentialSolver {
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
    }

    private static volatile AtomicBoolean isFinished = new AtomicBoolean();



    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
    }

    private ForkJoinSolver(Maze maze, int start, ConcurrentSkipListSet<Integer> visited, ConcurrentMap<Integer, Integer> predecessor) {
        this(maze);
        super.start = start;
        super.visited = visited;
        super.predecessor = predecessor;
    }



    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return the list of node identifiers from the start node to a
     * goal node in the maze; <code>null</code> if such a path cannot
     * be found.
     */
    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {
        // one player active on the maze at start
        // start with node given by constructor as 'start'.
        //'testandgrab' add start to visited.
        frontier.push(start);
        if (!visited.add(start)) {
            return null;
        }
        //we create a new player only if we succeeded in adding the start node to visited, this ensures that we don't spawn multiple players in the same cell.
        int player = maze.newPlayer(start);
        boolean firstRun = true;
        // as long as not all nodes have been processed nor the goal has been reached by some fork.


        while (!frontier.empty() && !isFinished.get()) {
            int current = frontier.pop();
            //This is used for runs subsequent to the first run.
            //Also adds current to visited, if it fails we proceed with the next node in frontier.
            if (!visited.add(current) && !firstRun) {
                continue;
            } else if (firstRun) {
                firstRun = false;
            }
            //Moves the player graphically.
            maze.move(player, current);

            // if current node has a goal
            if (maze.hasGoal(current)) {
                //set isFinished to true to tell all other threads to stop searching.
                isFinished.set(true);
                // search finished: reconstruct and return path
                return pathFromTo(maze.start(), current);
            }

            // If all neighbours are visited, e.g. when player reaches a dead end,
            // this path does not lead to goal and therefore should be discarded.
            if (allNeighboursVisited(current)) {
                return null;
            }

            // In case of only one possible path, do not spawn any new players, instead
            // let the current process deal with it.
            if (notVisitedNeighbours(current) <= 1) {
                // Only consider node if it has not been visited before
                // for every node nb adjacent to current
                for (int nb : maze.neighbors(current)) {
                    // add nb to the nodes to be processed
                    if (notInFrontierOrVisited(nb)) {
                        frontier.push(nb);
                        predecessor.put(nb, current);
                    }
                }
            } else {
                // Two or more unvisited neighbours exist
                // Create new player for each possible path
                List<ForkJoinSolver> subtasks = new ArrayList<>();
                for (int nb : maze.neighbors(current)) {
                    if (notInFrontierOrVisited(nb)) {
                        predecessor.put(nb, current);
                        subtasks.add(new ForkJoinSolver(maze,nb,visited,predecessor));
                    }
                }

                // Activate players
                for (ForkJoinSolver task : subtasks) {
                    task.fork();
                }

                // Collect result only if path exists, which it only will if
                // player found the goal.
                for (ForkJoinSolver subtask : subtasks) {
                    List<Integer> path = subtask.join();
                    if (path != null) {
                        return path;
                    }
                }

            }
        }

        // all nodes explored, no goal found
        return null;
    }

    protected List<Integer> pathFromTo(int from, int to) {
        List<Integer> path = new LinkedList<>();
        Integer current = to;
        while (current != from) {
            path.add(current);
            current = predecessor.get(current);
            System.out.println(current);
            if (current == null) {
                return null;
            }
        }
        path.add(from);
        Collections.reverse(path);
        return path;
    }

    /**
     * For current position, checks if all surrounding neighbours are visited.
     */
    private boolean allNeighboursVisited(int current) {
        return notVisitedNeighbours(current) == 0;
    }

    /**
     * Counts the number of neighbours that are not visited.
     */
    private int notVisitedNeighbours(int current) {
        int count = 0;
        for (int neighbor : maze.neighbors(current)) {
            if (notInFrontierOrVisited(neighbor))
                count++;
        }
        return count;
    }

    /**
     * Adds to visited and moves player on the maze if it has not been previously visited.
     */
    private void move(int player, int current) {
        visited.add(current);
        maze.move(player, current);
    }

    private boolean notInFrontierOrVisited(int node) {
        return !frontier.contains(node) && !visited.contains(node);
    }
}
