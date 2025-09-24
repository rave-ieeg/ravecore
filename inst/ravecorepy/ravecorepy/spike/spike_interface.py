# SpikeInterface imports
# We handle both newer and older class names to be robust across versions.

import numpy as np

def make_recording(traces, fs_hz, channel_ids = None, electrode_positions = None, path = None):
    import spikeinterface as si
    import spikeinterface.preprocessing as spre
    import spikeinterface.sorters as ss
    try:
        import spikeinterface.extractors as se
        HAS_NUMPY_RECORDING = hasattr(se, "NumpyRecording")
        HAS_NUMPY_RECORDING_EXTRACTOR = hasattr(se, "NumpyRecordingExtractor")
    except Exception:
        se = None
        HAS_NUMPY_RECORDING = False
        HAS_NUMPY_RECORDING_EXTRACTOR = False
        
    """Create a SpikeInterface Recording from a (num_samples, num_channels) numpy array."""
    if se is None:
        raise RuntimeError("Could not import spikeinterface.extractors properly.")
    if len(traces.shape) == 1:
        traces.reshape((-1, 1))
    if HAS_NUMPY_RECORDING:
        # Newer API
        rec = se.NumpyRecording(traces_list=[traces], sampling_frequency=fs_hz)
    elif HAS_NUMPY_RECORDING_EXTRACTOR:
        # Older API
        rec = se.NumpyRecordingExtractor(traces_list=[traces], sampling_frequency=fs_hz)
    else:
        # Fallback: try core API available in some versions
        try:
            from spikeinterface.core import NumpyRecording
            rec = NumpyRecording(traces_list=[traces], sampling_frequency=fs_hz)
        except Exception as e:
            raise RuntimeError(
                "Could not find a NumpyRecording class in spikeinterface. "
                "Please upgrade spikeinterface."
            ) from e
    # Provide basic channel locations and ids for completeness
    num_ch = traces.shape[1]
    if channel_ids is None:
        channel_ids = np.arange(num_ch)
    else:
        channel_ids = np.array(channel_ids).astype(int).reshape((-1))
      
    
    if hasattr(rec, "set_channel_ids"):
        rec.set_channel_ids(channel_ids)
    elif hasattr(rec, "rename_channels"):
        rec.rename_channels(channel_ids)
    
    if electrode_positions is None:
        electrode_positions = np.zeros((num_ch, 3))
    else:
        electrode_positions = np.array(electrode_positions)
        if len(electrode_positions.shape) == 1:
            electrode_positions.reshape((1, -1))
    
    if hasattr(rec, "set_channel_locations"):
        rec.set_channel_locations(electrode_positions.astype(float))
    if path is not None:
      # save to path
      rec.save_to_folder(folder = path, overwrite = True)
    return rec


def load_recording(path):
    import spikeinterface as si
    return si.load(path)

def combine_recording_by_channel(recordings, channel_ids = None):
    import spikeinterface as si
    if channel_ids is not None:
        channel_ids = np.array(channel_ids).astype(int).reshape((-1))
    rec_stacked = si.aggregate_channels(recordings, renamed_channel_ids = channel_ids)
    return rec_stacked

def combine_recording_by_time(recordings):
    import spikeinterface as si
    return si.append_recordings(recordings)


def bandpass(recording, freq_min=300.0, freq_max=6000.0):
    import spikeinterface.preprocessing as spre
    return spre.bandpass_filter(recording, freq_min=freq_min, freq_max=freq_max, dtype=np.float32)
  
def whiten(recording):
    import spikeinterface.preprocessing as spre
    return spre.whiten(recording)


def run_sorter(recording, sorter_name = "mountainsort5", folder = None, verbose=True, **kwargs):
    import spikeinterface.sorters as ss
    sorting = ss.run_sorter(
        sorter_name=sorter_name,
        recording=recording,
        folder=folder,
        remove_existing_folder=True,
        verbose=verbose,
        **kwargs
    )
    return sorting

def new_probe(positions, channel_ids, shapes="circle", radius = 0.01):
    from probeinterface import Probe
    channel_ids.reshape((-1)).astype(int)
    ndim = positions.shape[1]
    probe = Probe(ndim = ndim, si_units="mm")
    probe.set_contacts(
        positions=positions,
        shapes="circle",
        shape_params={"radius": radius}
    )
    probe.set_device_channel_indices(channel_ids)
    return probe

def new_probegroup(probe_list):
    from probeinterface import ProbeGroup
    pg = ProbeGroup()
    for probe in probe_list:
        pg.add_probe(probe)
    return pg

def set_probegroup(recording, pg, in_place=False):
    recording = recording.set_probegroup(pg, in_place=in_place)
    return recording

def set_channel_groups(recording, groups):
    groups = np.array(groups).reshape((-1)).astype(int)
    recording.set_channel_groups(groups)
    return recording

def plot_raster(sorting, title="Spike raster"):
    unit_ids = sorting.get_unit_ids()
    if len(unit_ids) == 0:
        print("No units found.")
        return
    import matplotlib.pyplot as plt
    fs_hz = sorting.get_sampling_frequency()
    trains_s = []
    for u in unit_ids:
        st = sorting.get_unit_spike_train(u, start_frame=0, end_frame=None)
        st_s = st.astype(np.float64) / float(fs_hz)
        trains_s.append((u, st_s))

    plt.figure(figsize=(10, 4 + 0.2 * len(unit_ids)))
    for i, (u, st_s) in enumerate(trains_s):
        y = np.full_like(st_s, i + 1, dtype=float)
        plt.vlines(st_s, i + 0.6, i + 1.4, linewidth=0.8)
        plt.plot(st_s, y, ".", markersize=2)

    plt.yticks(range(1, len(unit_ids) + 1), [f"unit{int(u)}" for u in unit_ids])
    plt.xlabel("time (s)")
    plt.ylabel("units")
    plt.title(title)
    plt.tight_layout()
    return plt
