package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;

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
        int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);
        // as long as not all nodes have been processed
        while (!frontier.empty()) {
            // get the new node to process
            int current = frontier.pop();

            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                tryToMove(player,current);

                // search finished: reconstruct and return path
                return pathFromTo(maze.start(), current);
            }

            // If all neighbours are visited, e.g. when player reaches a dead end,
            // try to move if it has not been visited before and return null
            // because this path does not lead to goal and therefore should be discarded.
            if (allNeighboursVisited(current)) {
                tryToMove(player,current);
                return null;
            }

            // In case of only one possible path, do not spawn any new players, instead
            // let the current process deal with it.
            if (notVisitedNeighbours(current) <= 1) {
                // Only consider node if it has not been visited before
                if (!visited.contains(current)) {
                    tryToMove(player,current);

                    // for every node nb adjacent to current
                    for (int nb : maze.neighbors(current)) {
                        // add nb to the nodes to be processed
                        if (!visited.contains(nb)) {
                            frontier.push(nb);
                            predecessor.put(nb, current);
                        }
                    }
                }
            } else {
                // Two or more unvisited neighbours exist
                tryToMove(player,current);

                // Create new player for each possible path
                List<ForkJoinSolver> subtasks = new ArrayList<>();
                for (int nb : maze.neighbors(current)) {
                        if (!visited.contains(nb)) {
                            predecessor.put(nb, current);
                            subtasks.add(new ForkJoinSolver(maze, nb, visited, predecessor));
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
            if (!visited.contains(neighbor))
                count++;
        }
        return count;
    }

    /**
     * Adds to visited and moves player on the maze if it has not been previously visited.
     */
    private void tryToMove(int player,int current){
        if (!visited.contains(current)){
            visited.add(current);
            maze.move(player,current);
        }
    }
}
