costStraight = 10;
costDiagonal = 15;
nodeStatusOpen = 1;
nodeStatusClosed = 0;

function clamp (v, min, max) {
    if (v > max) {
	return max;
    } else {
	if (v < min) {
	    return min;
	} else {
	    return v;
	}
    }
}

function makeGridLocation (v) {
    return {x: clamp(v.x, 0, games.robot_rampage.tile_map.map_width),
	    y: clamp(v.y, 0, games.robot_rampage.tile_map.map_height)}
}

function distance (vector1, vector2) {
    v1 = vector1.x - vector2.x;
    v2 = vector1.y - vector2.y;
    return Math.sqrt(v1 * v1 + v2 * v2);
}

function linearCost (node) {
    return distance(node.endNode.gridLocation, node.gridLocation);
}

nodeStatus = [];
nodeCosts =  [];
openList =   null;
tiles =      null;

function isWallTile (x, y) {
    if (x >= 0 && x < games.robot_rampage.tile_map.map_width &&
	y >= 0 && y < games.robot_rampage.tile_map.map_height) {
	return tiles[x][y] >= games.robot_rampage.tile_map.wall_tile_start;
    } else {
	return false;
    }
}

function isEq (node1, node2) {
    return node1.gridLocation.x == node2.gridLocation.x && node1.gridLocation.y == node2.gridLocation.y;
}

function pathNode (parentNode, endNode, gridLocation, cost) {
    node = {parentNode:   parentNode,
		gridLocation: makeGridLocation(gridLocation),
		endNode:      endNode,
		directCost:   cost,
		totalCost:    0};
    if (endNode) {
	node.totalCost = node.directCost + linearCost(node);
    }
    return node;
}

function hsSet (hs, k, v) {
    if (!hs[k.x]) {
	hs[k.x] = [];
    }
    hs[k.x][k.y] = v;
}

function hsGet (hs, k) {
    a = hs[k.x];
    if (a) {
	return a[k.y];
    }
}

function addNodeToOpenList (node) {
    index = 0;
    cost = node.totalCost;

    while (openList.length > index && cost < openList[index].totalCost) {
	index++;
    }

    openList.splice(index, 0, node);
    hsSet(nodeCosts, node.gridLocation, node.totalCost);
    hsSet(nodeStatus, node.gridLocation, nodeStatusOpen);
}

function findAdjacentNodes (currentNode, endNode) {
    adjacentNodes = [];

    x = currentNode.gridLocation.x;
    y = currentNode.gridLocation.y;

    upLeft = true;
    upRight = true;
    downLeft = true;
    downRight = true;

    if (x > 0 && !isWallTile(x - 1, y)) {
	adjacentNodes.push(pathNode(currentNode,
				    endNode,
				    {x: x - 1, y: y},
				    costStraight + currentNode.directCost));
    } else {
	upLeft = false;
	downLeft = false;
    }

    if (x < 49 && !isWallTile(x + 1, y)) {
	adjacentNodes.push(pathNode(currentNode,
				    endNode,
				    {x: x + 1, y: y},
				    costStraight + currentNode.directCost));
    } else {
	upRight = false;
	downRight = false;
    }

    if (y > 0 && !isWallTile(x, y - 1)) {
	adjacentNodes.push(pathNode(currentNode,
				    endNode,
				    {x: x, y: y - 1},
				    costStraight + currentNode.directCost));
    } else {
	upLeft = false;
	upRight = false;
    }

    if (y < 49 && !isWallTile(x, y + 1)) {
	adjacentNodes.push(pathNode(currentNode,
				    endNode,
				    {x: x, y: y + 1},
				    costStraight + currentNode.directCost));
    } else {
	downLeft = false;
	downRight = false;
    }

    if (upLeft && !isWallTile(x - 1, y - 1)) {
    	adjacentNodes.push(pathNode(currentNode,
    				    endNode,
    				    {x: x - 1, y: y - 1},
    				    costDiagonal + currentNode.directCost));
    }

    if (upRight && !isWallTile(x + 1, y - 1)) {
    	adjacentNodes.push(pathNode(currentNode,
    				    endNode,
    				    {x: x + 1, y: y - 1},
    				    costDiagonal + currentNode.directCost));
    }

    if (downLeft && !isWallTile(x - 1, y + 1)) {
    	adjacentNodes.push(pathNode(currentNode,
    				    endNode,
    				    {x: x - 1, y: y + 1},
    				    costDiagonal + currentNode.directCost));
    }

    if (downRight && !isWallTile(x + 1, y + 1)) {
    	adjacentNodes.push(pathNode(currentNode,
    				    endNode,
    				    {x: x + 1, y: y + 1},
    				    costDiagonal + currentNode.directCost));
    }

    return adjacentNodes;
}

function findPath(startTile, endTile, _tiles) {
    tiles = _tiles;
    if (isWallTile(startTile.x, startTile.y) || isWallTile(endTile.x, endTile.y)) {
	return null;
    }

    openList = [];
    nodeCosts = [];
    nodeStatus = [];

    endNode = pathNode(null, null, endTile, 0);
    startNode = pathNode(null, endNode, startTile, 0);

    addNodeToOpenList(startNode);

    while (openList) {
	currentNode = openList.pop();

	if (isEq(currentNode, endNode)) {
	    prev = currentNode;
	    while (currentNode.parentNode) {
		prev = currentNode;
		currentNode = currentNode.parentNode;
	    }
	    return prev.gridLocation;
	}

	hsSet(nodeCosts, currentNode.gridLocation, null);
	adjacentNodes = findAdjacentNodes(currentNode, endNode);
	for (i = 0; i < adjacentNodes.length; i++) {
	    possibleNode = adjacentNodes[i];
	    if (hsGet(nodeStatus, possibleNode.gridLocation) != undefined) {
		if (hsGet(nodeStatus, possibleNode.gridLocation) == nodeStatusClosed) {
		    continue;
		}

		if (hsGet(nodeStatus, possibleNode.gridLocation) == nodeStatusOpen) {
		    if (possibleNode.totalCost >= hsGet(nodeCosts, possibleNode.gridLocation)) {
			continue;
		    }
		}
	    }

	    addNodeToOpenList(possibleNode);
	}

	hsSet(nodeStatus, currentNode.gridLocation, nodeStatusClosed);
    }

    return null;
}
