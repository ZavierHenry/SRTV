import sys

import numpy as np
import soundfile as sf

import tensorflow as tf

from tensorflow_tts.inference import TFAutoModel
from tensorflow_tts.inference import AutoProcessor

# initialize models
fastspeech2 = TFAutoModel.from_pretrained("tensorspeech/tts-fastspeech2-ljspeech-en")
mb_melgan = TFAutoModel.from_pretrained("tensorspeech/tts-mb_melgan-ljspeech-en")

# inference
processor = AutoProcessor.from_pretrained("tensorspeech/tts-fastspeech2-ljspeech-en")

def inference(text, output_filename):
    input_ids = processor.text_to_sequence(text)
    samplerate = 22050

    _, mel_after, _, _, _ = fastspeech2.inference(
        input_ids=tf.expand_dims(tf.convert_to_tensor(input_ids, dtype=tf.int32), 0),
        speaker_ids=tf.convert_to_tensor([0], dtype=tf.int32),
        speed_ratios=tf.convert_to_tensor([1.0], dtype=tf.float32),
        f0_ratios=tf.convert_to_tensor([1.0], dtype=tf.float32),
        energy_ratios=tf.convert_to_tensor([1.0], dtype=tf.float32)
    )

    audio_after = mb_melgan.inference(mel_after)[0, :, 0]
    sf.write(output_filename, audio_after, samplerate, "PCM_16", format="WAV")
   

if __name__ == "__main__":
    text = sys.argv[1]
    output_filename = sys.argv[2]

    inference(text, output_filename)
    print(f'Finished writing {text} to {output_filename}...')