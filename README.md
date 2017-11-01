# CH/OTP Test Task
While the goal of the task is well defined, it still has a lot of moving parts. Therefore I did a number of assumptions that I will cover in this document.

# Usage

The project can be built with stack. The example startup script is provided (`run.sh`) for starting a few local nodes.

# Assumptions & Design
## Timestamps & clock synchronization

The clock synchronization problem in distributed systems is non-trivial and is often aided by use of logical clocks (Lamport clock, vector clock), but they still provide a partial ordering. In real world, function τ does not exist.

For the sake of simplicity I decided to rely on synchronization of wall-time clocks on the machines running nodes. I assume that clients will have NTP daemon running that will provide time synchronization in order of tens of miliseconds. However, we need to be also aware of not ideal clock accuracy (some clocks running slightly faster than others), so there will be some clock drift. Higher clock accuracy, if required, is often achieved by using GPS signal as precise clock source (apart of atomic clocks etc.).

## Message ordering

Using wall-time clock still has some gotchas – multiple messages may be sent with the same timestamp. Either, by multiple nodes, but also by single node if clock resolution is low (in my setup clock resolution reported by the system is 1 microsecond, not so bad).

If only timestamp was used for ordering messages, different scores may be computed for the same set of messages, depending on e.g. message arrival order. In order to improve consensus between nodes, extra assumption needs to be made.

The messages contain random number, timestamp, sequence number and sending node id. I make an extra assumption to order messages not only by timestamp, but also by sending node id and sequence number being sent from particular node. 

This guarantees single ordering for any set of messages. On the other hand, nodes with “lower” node id will have some preference over the others. This is not necessarily a good thing we want to have. Alternatively, we could sort messages by their value (random number) 

## Communication

In my implementation all nodes are equal, there is no single master node. All nodes are started independently. I decided to use peer-to-peer communication model (without using `distributed-process-p2p`  library which seems to be broken right now).

I assume that all nodes are reachable in terms of network connectivity (firewalls, NATs, broken routing). This assumption could be loosened by implementing some sort of routing algorithm if no direct communication is possible. In some non-trivial version that could use the similar principles as routing protocols used in networks (like RIP, OSPF or BGP).

## Node discovery

Point 2.3 of the task description suggests that nodes do not necessarily have upfront knowledge of other nodes in the network. To aid this issue, I implement simple (really) peer exchange protocol.

Each node may be provided with initial list of known nodes. To do this one needs to create a file in format `host:port` per line and it’s location is provided by `--peers-file knownPeers.txt`.

After startup, nodes try to connect with all nodes supplied. They do this by sending their known peers list to the other node. Each node, upon receiving known nodes from the other node merges it to local known peers list. If it was modified by doing that, it sends this merged version to all known peers. Ultimately, all nodes should have the same peer list. While this protocol is suboptimal as it involves more communication than necessary, it still does its job.

Additionally, known peers list is retransmitted to all known peers that have not yet responded a few times per second, to ensure a quick cluster warmup. 

Nodes start to send data just after startup. For testing purposes, I allow for a certain warmup period for peer exchange (configured with `--connect-for` option, defaults to 0). After this period data transmission starts. However, this is not required for data consistency, since I implement retransmissions (as long as data to be retransmitted is in local buffer).

## Retransmissions

Some initial messages may be lost, since nodes start to send messages before they connect to all other nodes. To aid this problem, each message contains sequence number. Data collectors keep track of last message number received from each node. If message loss is detected, receiving node asks the sending node to retransmit certain range of messages. Those messages are retreived from data collector buffer of sending node if they are still in “hot” buffer (haven’t been taken to compute partial result yet).

## Memory usage

In order to prevent memory leaks I use strict version of fold and strict constructor for collector state. This protects from accumulation of non-evaluated thunks.

Another concern is receiver message buffer. Receiver maintains a buffer of incoming messages represented as a set. To make it possible to have really long runs of the program, I implement special algorithm to precompute result on the fly.

For each known node, I compute maximum timestamp of continuous messages (means no message between was lost). Then I compute minimum of those (t_min). Since for all other nodes I have messages with newer timestamp, no messages from them will happen before t_min and I can safely compute partial result for all messages before it.
Precomputing of results happen when the buffer size exceeds threshold value.
The drawback of this solution is that after precomputing result it is impossible to retransmit messages which are no longer in message buffer.

This safe algorithm does not help if some nodes die and the buffer starts growing, since t_min will not be raising. In such case, there is another threshold and if it’s exceeded, I force precomputation of partial result for half that threshold. If missing messages are 
delivered after result is precomputed they are discarded.

## Random numbers

All nodes can use different DRNG seed and it can be configured with `--with-seed` option. 

## Trailing message

Nodes send extra message upon finishing sending messages. When receiver gets such message from all nodes it can compute and display final result. Otherwise, the result will be computed just before final deadline.

